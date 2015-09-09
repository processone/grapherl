import os
import sys

VSN_OLD = sys.argv[1]
VSN_NEW = "0.2.1"


print("Upgrading from ", VSN_OLD, " to ", VSN_NEW, "(press enter to continue) ", end="")
input()


os.system("cp apps/graph_db/src/graph_db.appup apps/graph_db/ebin/graph_db.appup")
os.system("cp apps/graph_web/src/graph_web.appup apps/graph_web/ebin/graph_web.appup")
os.system("cp apps/grapherl/src/grapherl.appup apps/grapherl/ebin/grapherl.appup")


# build the new release
os.system("make")

# generate relup w.r.t to the previous release
os.system("rebar3 relup -n grapherl -v " + VSN_NEW + " -u " + VSN_OLD)

# genereate tar file of the new release
os.system("rebar3 tar -n grapherl -v " + VSN_NEW)

# move the generated tar file to destination folder
os.system("mv _build/default/rel/grapherl/grapherl-" +
          VSN_NEW + ".tar.gz _build/default/rel/grapherl/releases/" + VSN_NEW +"/grapherl.tar.gz")

# upgrade to the new release
os.system("_build/default/rel/grapherl/bin/grapherl-" + VSN_OLD + " upgrade " + VSN_NEW)

os.system("mkdir _build/default/rel/grapherl/lib/graph_web-" + VSN_NEW + "/priv")
os.system("cp -R apps/graph_web_client/build/* _build/default/rel/grapherl/lib/graph_web-" + VSN_NEW + "/priv ")
