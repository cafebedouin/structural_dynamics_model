% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionist Legitimacy Basis — National Liberation Reading
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This story authors the national-liberation reading of the contested
 *   Zionist legitimacy kernel: Zionism as the political movement of a
 *   persecuted, indigenous-connected people returning to and reconstituting
 *   sovereignty in its ancestral homeland after millennia of exile and
 *   recurring violence, most acutely the Holocaust. Under this reading, the
 *   persecution history and the historical-religious continuity of Jewish
 *   presence in the land together generate a legitimacy claim strong enough
 *   to justify establishing a Jewish-majority state even where this required
 *   displacing or subordinating the existing Arab population — and Arab and
 *   Palestinian opposition to this project is read, within the reading's own
 *   logic, primarily as illegitimate denial of Jewish national rights rather
 *   than as a competing indigenous claim of equal standing. This is exactly
 *   one reading among three sibling readings of the same kernel
 *   (national_liberation_reading, religious_restoration_reading,
 *   settler_colonial_reading); this file does not adjudicate between them,
 *   does not average their claims, and does not describe the contest itself
 *   inside its own metrics. Its epsilon is authored for the standing
 *   arrangement — the national movement and the state it produced, evaluated
 *   by this reading's own internal premises — not for an idealized
 *   coexistence outcome this reading might endorse.
 *
 * KEY AGENTS:
 *   - jewish_national_home_settlers: Primary organizing beneficiary (organized/constrained) — builds the national project on the persecution-and-return premise
 *   - post_shoah_jewish_refugees: Powerless beneficiary (powerless/trapped) — the movement's most urgent moral referent
 *   - israeli_state_institutions: Agenda-setting institutional beneficiary (institutional/arbitrage) — codifies and enforces the reading
 *   - palestinian_arab_residents_1917_1948 and palestinian_refugees_1948: Primary payers (powerless/trapped) — bear the displacement this reading frames as secondary or self-inflicted
 *   - arab_states_and_pan_arab_nationalist_movements: Excluded voice — their competing self-determination claim is treated as rejectionism within this reading's premises
 *   - international_diaspora_and_human_rights_observers: Analytical observer — documents outcomes both readings selectively cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.58).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.62).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionist Legitimacy Basis — National Liberation Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, 'b7ce49bb-e8d3-4d37-8809-3a74c0b978c1').
narrative_ontology:cs_kernel_codification('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', distributed).
narrative_ontology:cs_authority_grounding('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', distributed).
narrative_ontology:cs_reading_relation('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', foundational, persecution_history_grounds_return_legitimacy).
narrative_ontology:cs_axiom_status(persecution_history_grounds_return_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', persecution_history_grounds_return_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', foundational, arab_opposition_constitutes_denial_of_jewish_national_rights).
narrative_ontology:cs_axiom_status(arab_opposition_constitutes_denial_of_jewish_national_rights, holdable).
narrative_ontology:cs_axiom_grounding('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', arab_opposition_constitutes_denial_of_jewish_national_rights, conventional).
narrative_ontology:cs_reference_frame('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', persecution_driven_national_return_legitimacy).
narrative_ontology:cs_drift_state('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', post_1993_oslo_and_contemporary_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b7ce49bb-e8d3-4d37-8809-3a74c0b978c1', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_national_home_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, post_shoah_jewish_refugees).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_residents_1917_1948).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_population_under_ongoing_control).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, jewish_indigenous_connection_to_land).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, persecution_justifies_national_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish immigrants and their descendants building agricultural settlements, cities, and political institutions in Mandate Palestine and later Israel, understanding themselves as a persecuted people returning to ancestral land after millennia of exile and escalating European antisemitism. They organize land purchase, self-defense militias, and eventually state institutions on the premise that national return is the only durable remedy to persecution.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_national_home_settlers, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, jewish_national_home_settlers, agenda_setter).

% Survivors of European genocide and displaced persons camps with no state willing to absorb them in the numbers needed, for whom the national home represents the only available escape from statelessness and renewed persecution. Their situation is used to justify urgency in immigration and settlement that outpaces negotiated accommodation with existing residents.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, post_shoah_jewish_refugees, beneficiary,
    powerless, biographical, trapped, regional).

% The state apparatus that codifies the national liberation narrative into law (Law of Return, national symbols, founding documents), administers land and citizenship policy on its basis, and enforces the boundaries of legitimate political claim-making, including which counter-narratives are treated as denial of Jewish rights versus legitimate grievance.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_state_institutions, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The Arab majority population resident in Palestine before and during the Mandate period, whose land purchase displacement, political exclusion from self-determination structures, and eventual mass displacement in 1947-49 are reframed under this reading as the necessary or regrettable but secondary cost of a prior people's return, or as their own responsibility for rejecting partition and going to war.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_residents_1917_1948, payer,
    powerless, generational, trapped, regional).

% Those expelled or fled during 1947-49 and their descendants, denied a right of return that mirrors the return-claim made on their behalf's behalf for Jewish settlers; their loss is structurally minimized or attributed to Arab state rejectionism within this reading, and their political claims are treated as a competing rather than co-equal grievance.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees_1948, payer,
    powerless, civilizational, trapped, regional).

% Palestinians living under continued Israeli sovereignty, occupation, or blockade whose present-day claims to land, movement, and self-determination are evaluated, within this reading, against the prior legitimacy of Jewish return — meaning contemporary Palestinian political demands are read through a lens that treats them as secondary to, or in tension with, a prior indigenous claim.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_population_under_ongoing_control, payer,
    powerless, generational, trapped, regional).

% Regional states and movements that rejected partition and framed Zionism as foreign implantation; this reading treats their opposition primarily as evidence of rejectionism and antisemitism rather than as a competing account of self-determination, and their historical arguments are largely absent from the reading's own internal justification structure.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_states_and_pan_arab_nationalist_movements, excluded,
    powerful, generational, constrained, regional).

% UN bodies, historians, and human rights organizations that assess competing narrative claims, produce documentation (e.g. on 1948 displacement, ongoing occupation conditions), and whose findings are cited selectively by all readings in the kernel contest.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_diaspora_and_human_rights_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a persecuted, stateless people with a coherent basis for organizing collective self-defense, immigration absorption, and eventual sovereign self-determination, solving the genuine problem that no other state reliably protected Jews from recurring violence and that minority status under others' sovereignty had repeatedly proven fatal.
% TRANSFER_FUNCTION: Moves land, political sovereignty, demographic majority status, and the international legitimacy of national self-determination claims from the pre-existing Arab population of Palestine to the incoming and organizing Jewish national community, justified by prior historical-religious connection and the urgency of persecution.
% ABSENT_VOICES: Palestinian Arab residents and refugees are treated within this reading as objectors to a prior legitimate claim rather than as a people with an equally originary connection to the land; their own national narrative is structurally subordinated rather than engaged as a co-equal account, and this subordination is built into the reading's own premises rather than argued case by case.
% DISAPPEARANCE_RATIONALE: If the national-liberation legitimacy basis were to lose its organizing force, the entire architecture built on it — the Law of Return, the state's self-understanding, the international coalition that grounds recognition partly in Holocaust-era moral urgency, and the delegitimization of Palestinian counter-claims as denial rather than competing grievance — would require renegotiation; Israeli sovereignty would likely persist as a fact on the ground, but its normative justification and the treatment of Palestinian claims within it would shift substantially.
% FOUNDING_PROBLEM: Recurring, escalating, ultimately genocidal persecution of Jews in Europe and elsewhere, combined with the practical failure of emancipation, assimilation, and minority-rights frameworks to protect Jewish communities, created the problem this movement was built to solve: the absence of any sovereign territory where Jews could not be expelled, murdered, or subjected to statelessness at the whim of a host population or state.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Holocaust and of pre-state Jewish immigration (including scholars outside explicitly Zionist institutions) corroborate that statelessness and persecution were real and that available international protections failed catastrophically. However, historians of the Nakba and Palestinian scholars, along with some Israeli 'new historians' (e.g. Benny Morris, Ilan Pappé) writing from outside the movement's own institutions, corroborate that the same founding problem was addressed in a manner that produced a parallel and unresolved dispossession — meaning the founding problem's validity as a justification for the specific costs imposed is corroborated only partially, and its 'solved' status is disputed by those who bore the displacement costs.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 — substantial but not maximal — because the reading's own internal account genuinely does contain a real coordination function (protection of a stateless, persecuted people) alongside the asymmetric cost imposed on the pre-existing population, and this reading's own moral force (the Holocaust, centuries of persecution) is real and not fabricated. Suppression is authored higher (0.62) and reflects the active enforcement required to sustain the reading against a live counter-narrative: legal and rhetorical suppression of the Palestinian return claim, land and citizenship policy, and continued political and physical control mechanisms. Resistance is authored high (0.75) because Palestinian political mobilization, international legal challenge, and historiographic revision have been continuous and substantial since at least 1948. Accessibility collapse is moderate (0.45) — the reading has not fully foreclosed rival accounts (both within Israeli society and internationally, competing narratives remain visible and contested), distinguishing it from a mountain-like naturalized claim.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (settlers, refugees, state institutions) the constraint is experienced and narrated as rope or even mountain-adjacent — an inevitable, morally necessary response to persecution that required no active suppression of anyone's legitimate rights, because the opposing claim is not read as legitimate within this frame. From the payer seats the same structure computes as substantially extractive and actively enforced, because their competing indigenous claim is treated as subordinate by the very legitimacy basis this reading authors. The engine's per-seat computation is expected to surface exactly this divergence; this story does not attempt to resolve it by picking a single 'true' seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settlers, post-Shoah refugees, and israeli state institutions sit toward the beneficiary end: the constraint (the legitimacy basis itself, and the political-legal structure built on it) subsidizes their claims to land, citizenship, and international recognition. Palestinian residents, 1948 refugees, and the population under ongoing control sit toward the target end: the same legitimacy basis is the instrument by which their competing claim is structurally subordinated, and their exit options are trapped rather than mobile — statelessness, refugee status, or continued residence under a sovereignty whose foundational narrative denies the co-equal standing of their own claim. Arab states are excluded rather than coordinated: their opposition is treated by the reading's own logic as illegitimate rather than as a rival account meriting negotiation on equal terms, which is precisely the expected structural delta of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the near-total failure of the international order to protect Jews from persecution and genocide, and the absence of any state that would reliably guarantee Jewish safety — was genuinely live at the movement's founding and catastrophically vindicated by the Holocaust; this reading is not manufacturing a founding problem. But the founding problem's status is authored as contested rather than resolved: statehood substantially addressed the original problem (a sovereign refuge now exists), while the specific mechanism chosen to solve it produced a second, unresolved displacement whose costs this reading's own premises treat as secondary. Classifying this as tangled_rope rather than snare or rope preserves both halves: it prevents mislabeling a movement with a genuine, historically vindicated coordination function as pure extraction, while also refusing to launder the asymmetric and continuing cost borne by Palestinian residents and refugees as if it were incidental to a purely coordinative arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which reading of the zionist_legitimacy_basis kernel (national_liberation, religious_restoration, or settler_colonial) best accounts for the historical evidence of Zionist movement founders'' own stated motivations and strategic choices?',
    'Comparative historiographic analysis of primary sources across the movement''s political (Herzl, Jabotinsky), religious (Rav Kook), and practical-settlement (labor Zionist) strands would show whether persecution-avoidance, religious fulfillment, or colonial-settlement logics dominate the documentary record, and whether these logics were treated by founders as complementary or competing.',
    'If persecution-avoidance dominates the documentary record, this reading''s legitimacy claim strengthens relative to the religious and settler-colonial readings; if colonial-settlement strategic logic (land acquisition patterns, labor exclusion policy, demographic engineering) dominates independent of stated persecution rationale, the settler_colonial_reading gains relative evidentiary weight and this reading''s framing of Arab opposition as mere denial becomes harder to sustain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading the founding documentary record best supports.').

omega_variable(
    persecution_urgency_versus_displacement_cost_proportionality,
    'Does the severity and inescapability of Jewish persecution (culminating in the Holocaust) justify a national solution imposed at the cost of displacing an existing population, or does it justify only a claim to refuge without justifying the specific costs imposed on that population?',
    'This is not resolvable by additional historical fact-finding alone; it depends on which theory of national self-determination and proportionality between historic injury and remedy one adopts, though comparative analysis of how other post-genocide or post-persecution national projects were and were not permitted to impose costs on third parties could inform the comparison.',
    'If proportionality is required and was exceeded, this reading''s delegitimization of Arab/Palestinian opposition as mere denial becomes structurally weaker even on the reading''s own premises; if urgency is taken to override proportionality entirely, the reading''s internal logic is more fully self-consistent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persecution_urgency_versus_displacement_cost_proportionality, preference, 'Whether persecution severity justifies the specific displacement costs this reading treats as secondary.').

omega_variable(
    indigenous_connection_claim_strength,
    'Does documented historical-religious continuity of Jewish presence and connection to the land (versus continuous physical residence by the pre-1948 Arab population) support a co-equal or superior indigenous claim under this reading''s own terms?',
    'Archaeological, demographic, and continuous-residence historical record analysis, combined with comparative analysis of how ''indigeneity'' is defined and adjudicated in other contested-land contexts internationally (e.g. by UN indigenous rights frameworks).',
    'A finding that indigeneity claims require continuous physical presence rather than historical-religious connection would weaken this reading''s core premise; a finding that historical-religious continuity is sufficient would strengthen it relative to the settler_colonial_reading''s characterization of the movement as externally implanted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_connection_claim_strength, empirical, 'Whether historical connection versus continuous residence grounds the stronger indigeneity claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1897, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1897, 0.05).
narrative_ontology:measurement(zion_tr_t1917, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1917, 0.08).
narrative_ontology:measurement(zion_tr_t1936, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1936, 0.12).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1993, 0.2).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(zion_be_t1897, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1897, 0.15).
narrative_ontology:measurement(zion_be_t1917, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1917, 0.28).
narrative_ontology:measurement(zion_be_t1936, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1936, 0.4).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.62).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1897, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1897, 0.1).
narrative_ontology:measurement(zion_su_t1917, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1917, 0.25).
narrative_ontology:measurement(zion_su_t1936, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1936, 0.45).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the zionist_legitimacy_basis kernel. national_liberation_reading grounds legitimacy in persecution history and historical-religious connection, producing epsilon 0.58 (substantial but partially coordinative). religious_restoration_reading grounds legitimacy in divine covenant and messianic fulfillment, which would produce a different beneficiary/victim structure emphasizing religious settler movements post-1967 and a different, likely higher, extraction profile given territorial maximalism. settler_colonial_reading grounds the same historical events in colonial-settlement theory, which would produce a substantially higher epsilon and a claimed_type of snare or tangled_rope with a starkly different coordination-function narrative (denying that genuine persecution-based coordination is the operative mechanism at all). Each reading is authored as its own ε-invariant constraint; none averages or references the others' internal metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
