/* tslint:disable */
export function view_map_length(arg0: ViewMap): number;

export function view_map_x(arg0: ViewMap): number;

export function view_map_y(arg0: ViewMap): number;

export function view_map_z(arg0: number, arg1: ViewMap): number;

export function view_map_views(arg0: number, arg1: ViewMap): Views;

export function views_length(arg0: Views): number;

export function views_eq(arg0: Views, arg1: Views): boolean;

export function view_is_image(arg0: number, arg1: Views): boolean;

export function view_image_name(arg0: number, arg1: Views): string;

export function view_image_x(arg0: number, arg1: Views): number | undefined;

export function view_image_y(arg0: number, arg1: Views): number | undefined;

export function view_image_z(arg0: number, arg1: Views): number | undefined;

export function new_(): Game;

export function step(arg0: number, arg1: any, arg2: Game): Game;

export function view_map(arg0: Game): ViewMap;

export class Game {
free(): void;

}
export class ViewMap {
free(): void;

}
export class Views {
free(): void;

}
