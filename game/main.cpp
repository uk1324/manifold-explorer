#include <engine/Engine.hpp>
#include <engine/EngineUpdateLoop.hpp>

#ifdef FINAL_RELEASE
#define FONT "assets/fonts/RobotoMono-Regular.ttf"
#else 
#define FONT "engine/assets/fonts/RobotoMono-Regular.ttf"
#endif


int main() {
 	Engine::initAll(Window::Settings{
		.maximized = true,
		.multisamplingSamplesPerPixel = 16
	}, FONT);

	EngineUpdateLoop updateLoop(60.0f);

	while (updateLoop.isRunning()) {
		
	}

	Engine::terminateAll();
}

#ifdef FINAL_RELEASE

#ifdef WIN32
#include <Windows.h>
int WINAPI WinMain(_In_ HINSTANCE, _In_opt_ HINSTANCE, _In_ LPSTR, _In_ int) {
	return main();
}
#endif

#endif