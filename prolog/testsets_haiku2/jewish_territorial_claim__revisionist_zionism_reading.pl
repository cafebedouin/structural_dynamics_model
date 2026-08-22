% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Maximalist Territorial Claim with Military Enforcement (Revisionist Zionism Reading)
 *   domain: political/historical/nationalist
 *
 * SUMMARY:
 *   The revisionist Zionism reading of the contested kernel 'Jewish
 *   territorial claim' asserts a maximalist territorial demand (both banks of
 *   the Jordan River) as non-negotiable and frames military force ('Iron
 *   Wall' doctrine) as the primary mechanism to compel Arab acceptance. This
 *   reading explicitly rejects Arab consent as a prerequisite for Jewish
 *   sovereignty — instead, Arab resistance is treated as inevitable and the
 *   constraint's function is to overcome that resistance through superior
 *   military power. The reading fuses Jewish national identity with
 *   territorial maximalism and military domination, making the constraint's
 *   core claim that Jewish security and dignity require not negotiated
 *   coexistence but enforced dominance. The boundary between coordination
 *   (solving the Jewish Question through secure territory) and extraction
 *   (denying Palestinians agency and Jordanian statehood) is deliberately
 *   collapsed — the constraint makes Palestinian suppression a structural
 *   necessity of the solution.
 *
 * KEY AGENTS:
 *   - Revisionist Zionist Movement (Ze'ev Jabotinsky faction): agenda-setter, organized power, identity-locked commitment to Iron Wall doctrine and territorial maximalism
 *   - Jewish Settler Population: beneficiary, moderate power, identity constituted through settlement and territorial claim, trapped exit via ideological fusion
 *   - Palestinian Arab Population: victim/payer, powerless, trapped exit, structural exclusion from determining political status
 *   - Jordanian State: victim/payer, powerful but constrained, targeted by the maximal claim, forced into armed response or capitulation
 *   - Arab States Coalition: payers and excluded voices, organized power, forced into role as 'Iron Wall' resistance to be suppressed
 *   - Labor Zionist Alternative Movement: excluded voices within Jewish institutional politics, rejected gradualism and consent-seeking
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.87).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.91).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.84).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Maximalist Territorial Claim with Military Enforcement (Revisionist Zionism Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political/historical/nationalist").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '4fe627e3-fb51-40a4-a7af-b0cc10a42242').
narrative_ontology:cs_kernel_codification('4fe627e3-fb51-40a4-a7af-b0cc10a42242', distributed).
narrative_ontology:cs_authority_grounding('4fe627e3-fb51-40a4-a7af-b0cc10a42242', extraction).
narrative_ontology:cs_reading_relation('4fe627e3-fb51-40a4-a7af-b0cc10a42242', jewish_territorial_claim__labor_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('4fe627e3-fb51-40a4-a7af-b0cc10a42242', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('4fe627e3-fb51-40a4-a7af-b0cc10a42242', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('4fe627e3-fb51-40a4-a7af-b0cc10a42242', foundational, arab_consent_unnecessary_for_jewish_sovereignty).
narrative_ontology:cs_axiom_status(arab_consent_unnecessary_for_jewish_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('4fe627e3-fb51-40a4-a7af-b0cc10a42242', arab_consent_unnecessary_for_jewish_sovereignty, deontological).
narrative_ontology:cs_axiom('4fe627e3-fb51-40a4-a7af-b0cc10a42242', foundational, territorial_maximalism_non_negotiable).
narrative_ontology:cs_axiom_status(territorial_maximalism_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('4fe627e3-fb51-40a4-a7af-b0cc10a42242', territorial_maximalism_non_negotiable, deontological).
narrative_ontology:cs_axiom('4fe627e3-fb51-40a4-a7af-b0cc10a42242', secondary, military_force_primary_enforcement_mechanism).
narrative_ontology:cs_axiom_status(military_force_primary_enforcement_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('4fe627e3-fb51-40a4-a7af-b0cc10a42242', military_force_primary_enforcement_mechanism, instrumental).
narrative_ontology:cs_reference_frame('4fe627e3-fb51-40a4-a7af-b0cc10a42242', jewish_historical_right_and_military_strength).
narrative_ontology:cs_drift_state('4fe627e3-fb51-40a4-a7af-b0cc10a42242', id_1948_statehood_moment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4fe627e3-fb51-40a4-a7af-b0cc10a42242', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_settler_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, jordanian_state).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_diaspora_communities).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, arab_states_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the territorial claim and the enforcement strategy. Argues that Jewish historical and religious right to both banks of the Jordan is non-negotiable and that Arab acceptance must be compelled through military strength ('Iron Wall' doctrine), not negotiated. The movement's identity and legitimacy are fused with the maximal territorial demand and the rejection of Arab consent as a prerequisite.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, agenda_setter,
    organized, generational, identity_locked, regional).

% Receives territorial settlement rights, state resources, and security guarantees under the claim. Identity as settlers is constituted through the territorial maximalism; exit would require abandoning the ideological and physical project of settlement. Benefits accrue through land allocation, military protection, and institutional preference, all justified by the territorial claim.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_settler_population, beneficiary,
    moderate, generational, identity_locked, regional).

% Bears the costs of territorial dispossession, displacement, and subjection to military rule. The constraint operates by denying them voice in determining their own political status — the territorial claim and enforcement mechanism make Arab consent structurally irrelevant. Their resistance is suppressed through military force and administrative control.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population, payer,
    powerless, biographical, trapped, regional).

% Is targeted by the maximal claim (both banks of Jordan). The constraint's assertion of Jewish sovereignty over the East Bank directly contradicts Jordanian state sovereignty. Jordan's options are military resistance (costly and unequal), diplomatic acceptance of a reduced state, or external arbitration it does not control.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jordanian_state, payer,
    powerful, generational, constrained, regional).

% Are positioned as targets of the 'Iron Wall' enforcement strategy. The constraint's logic treats Arab military resistance as inevitable and requires it to be overcome by superior force. Their exclusion from the territorial settlement (they have no say in the claim) is paired with their inclusion as the force to be suppressed.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, arab_states_coalition, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, arab_states_coalition, excluded).

% Is structurally sidelined by the reading's framing. The claim to sovereignty by military force sits in tension with UN frameworks on territorial change and self-determination, but the revisionist reading treats international legitimacy as secondary to military facts on the ground.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_law_framework, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(jewish_territorial_claim__revisionist_zionism_reading, international_law_framework).

% Are invoked as the ultimate beneficiary (the Jewish Question is supposedly solved by secure territory), but do not bear the direct costs of enforcement or displacement. Many diaspora members support the territorial claim ideologically but do not live under its enforcement apparatus. Their mobilization as a constituency legitimates the movement internationally.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_diaspora_communities, beneficiary,
    moderate, generational, mobile, global).

% Is excluded from the territorial settlement because the revisionist reading rejects the labor-zionist path of gradual fact-building and Arab coexistence. Labor zionists advocated building Jewish society through work and eventually negotiating with Arab counterparts; the revisionist reading treats such gradualism and consent-seeking as weakness. Labor zionists' voice is suppressed within Jewish institutional politics by the revisionist movement's assertion of the Iron Wall doctrine.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, labor_zionist_alternative_movement, excluded,
    organized, generational, constrained, regional).

% Formulate and propagate the ideological apparatus justifying military enforcement and territorial maximalism. Figures like Ze'ev Jabotinsky articulate the Iron Wall doctrine as a philosophical position. Their role is to provide narrative coherence and intellectual legitimacy to the constraint, transforming a territorial claim backed by force into a coherent nationalist ideology.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_intellectuals_and_leadership, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_intellectuals_and_leadership, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish national aspirations around maximalist territorial claim and military enforcement strategy; solves the internal Jewish organizational problem of unifying competing Zionist factions around an unified (maximalist) vision of Jewish statehood.
% TRANSFER_FUNCTION: Transfers Palestinian-inhabited land to Jewish settlers and the revisionist Zionist state; transfers political authority from Palestinian and Jordanian hands to Jewish institutional control; transfers the cost and burden of military resistance-suppression onto Palestinian and Arab populations; transfers legitimacy within Jewish diaspora constituencies from labor-zionist gradualism to revisionist maximalism.
% ABSENT_VOICES: Palestinian Arabs (complete exclusion — framed as objects of enforcement, not subjects of territorial determination); Jordanian state and Arab states (excluded from the territorial settlement, included only as objects of military suppression); labor-zionist alternative vision (marginalized within Jewish institutional politics); international law frameworks emphasizing self-determination and consent (sidelined as irrelevant).
% DISAPPEARANCE_RATIONALE: If the constraint vanished — if the maximal territorial claim and Iron Wall doctrine were abandoned — the entire political infrastructure would reorganize: Palestinian political agency and territorial determination would shift from being suppressed to being autonomous; Jordanian and Arab state interests would no longer be framed as illegitimate obstacles; labor-zionist and other alternative visions of Jewish-Arab coexistence would become live political options within Jewish institutional life; international law and diplomatic frameworks would become the governing mechanism rather than military facts on the ground.
% FOUNDING_PROBLEM: Jewish diaspora vulnerability, antisemitism, and the existential threat of Jewish dispersion and marginalization — framed as the 'Jewish Question': only complete territorial sovereignty over maximalist territory, secured by military dominance, can guarantee Jewish survival and dignity.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist Zionist leadership (Ze'ev Jabotinsky and successors) attest that the founding problem (Jewish existential vulnerability) remains live and requires maximal territorial security and military enforcement. Outside the revisionist movement, labor zionists, political zionists, and international observers attest that the founding problem is addressable through negotiated Jewish statehood, international guarantees, and Jewish integration into the family of nations, without requiring Arab suppression or territorial maximalism. Post-Holocaust Jewish communities and institutions increasingly attest that Jewish security is advanced through international law, institutional integration, and Holocaust memory and prevention, not through continued military domination of Arabs. By 1948, the shift in corroborating testimony is substantial: the founding problem's severity is no longer disputed, but its solutions are increasingly contested — international recognition of Jewish statehood (achieved via UN Partition Plan, 1947) provides an alternative to the Iron Wall doctrine.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.87) and rising over the interval because the constraint's operation requires continuous dispossession and territorial expansion — it is not a stable equilibrium but a dynamic seizure. The territorial claim cannot be separated from its enforcement; the extraction IS the enforcement. Suppression is even higher (0.91) because the constraint's persistence depends entirely on military dominance suppressing Palestinian and Arab resistance — there is no voluntary cooperation mechanism, no residual consent framework. Theater is low (0.22) because the Iron Wall doctrine is explicitly anti-performative: Jabotinsky and the revisionist leadership openly argue that moral or legal justification is irrelevant, that force alone matters, and that Arab acceptance cannot and need not be negotiated. The constraint is therefore mostly functional (enforcement) and minimally theatrical — there is little pretense of voluntary coordination. The measurement series track the intensification of both extractiveness and suppression over the 1920-1948 interval: as the settler population grew, territorial claims expanded, and Arab resistance mounted, the constraint required escalating military suppression to maintain the territorial seizure. The rising suppression_requirement reflects the historical intensification of conflict and the growing gap between the claim and Arab acceptance of that claim.
 *
 * PERSPECTIVAL GAP:
 *   The Palestinian and Jordanian seats would read this constraint as pure Snare — asymmetric extraction, coercive suppression, structural exclusion. The revisionist Zionist agenda-setter reads it as Rope — a coordination solution to the Jewish Question that happens to require overcoming Arab resistance. The labor-zionist seat (marginalized but present) reads it as corrupted Rope — the coordination function (Jewish national regeneration) is real but the extraction mechanism (Arab suppression rather than coexistence) is a historical error and a moral catastrophe. The engine computing per-seat classifications would produce: Snare for Palestinian seat (high extraction + high suppression + trapped exit), Snare for Jordanian seat (territorial threat + asymmetric costs), Rope (or contested hybrid) for revisionist agenda-setter seat (they author it as solving a problem with coordinated Jewish action), Rope-corrupted or Tangled-Rope for labor-zionist internal seat (coordination + extraction in one structure, but rejecting the extraction as unnecessary). This perspectival divergence is the analytical point — the constraint's classification depends on the seat, and the seats cannot converge on a unified reading because their structural relationships to the constraint are incommensurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Revisionist Zionist Movement: d ≈ 0.05 (full beneficiary — the constraint is authored by and for this actor, carries their identity, delivers their territorial and security aims). Jewish Settler Population: d ≈ 0.15 (beneficiary with modest costs — they receive land and security but bear some exposure to Arab military resistance; identity-locked exit keeps them bound to the constraint even when costs rise). Palestinian Arab Population: d ≈ 0.98 (full target — the constraint operates by denying them political agency, seizing their territory, and suppressing their resistance; trapped exit means they cannot exit except through displacement or political elimination). Jordanian State: d ≈ 0.92 (nearly full target — the maximal claim directly threatens their sovereignty; constrained exit means they face impossible choices of capitulation, armed resistance, or external arbitration). Arab States Coalition: d ≈ 0.88 (high target — their role is scripted as the force to be suppressed; exclusion from the territorial settlement means they absorb all costs of resistance with no voice in the outcome). The directionality curve is steep and asymmetric because the constraint's structure requires this: beneficiary seats have arbitrage and mobile exit; victim seats have trapped and identity-locked constraints that prevent escape. No overrides needed — the structural data (beneficiary/victim declarations + exit options) derives the correct d values directly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no mandatrophy — its founding problem (Jewish security, the Jewish Question) remains live and contested, and the constraint persists because beneficiary seats (revisionist movement and settlers) continue to invest in defending it. However, the constraint sits at a critical juncture (the 1948 endpoint): the founding problem's resolution-status is shifting. Pre-1945, the constraint's justification rested partly on genuine Jewish vulnerability and absence of alternatives (the Jewish Question appeared unsolvable by non-territorial means). Post-Holocaust, the constraint's claim to solve Jewish security through maximalist territory grows stronger emotionally but weaker empirically — established Jewish communities in diaspora, the international legitimacy of Jewish self-determination, and the emergence of the State of Israel (as a political fact, even if not maximalist) provide alternatives to the Iron Wall logic. The constraint does NOT decay into Piton at the interval's end (it is not performed theater maintained by inertia), but the measurement trajectory shows the constraint reaching an inflection point where its persistence will depend increasingly on active enforcement against rising resistance rather than on continuing to solve a live problem. A post-1948 story would likely track mandatrophy emergence as the founding problem's resolution-status shifts from live to contested to dead (Jewish security is no longer framed as unsolvable except through territorial maximalism), while the constraint persists via inertia and institutional commitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_resolution_status,
    'Is the Jewish Question (diaspora vulnerability, lack of secure territory) adequately addressed by the revisionist-maximalist constraint, or by alternative arrangements (negotiated Jewish state, international guarantees, diaspora integration)?',
    'Long-term observation (post-1948 interval and beyond) of whether Jewish security and dignity are achieved through the constrained territorial arrangement or require continuous military enforcement. Comparative analysis with Jewish communities in diaspora: do they report greater security under the maximal territorial claim than under institutional integration and international law protection?',
    'If the founding problem is shown to be resolvable without territorial maximalism and military enforcement, the constraint shifts from necessary-solution to extractive-rent-seeking, and mandatrophy accelerates. If alternative arrangements fail and only the maximal claim provides security, the constraint''s justification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_resolution_status, empirical, 'Whether the constraint adequately solves the founding problem or whether it persists beyond the problem''s resolution.').

omega_variable(
    iron_wall_doctrine_necessity,
    'Is military enforcement and the rejection of Arab consent structurally necessary for Jewish territorial claim, or is it a choice among available alternatives (negotiation, partition, federal structures, international arbitration)?',
    'Counterfactual historical analysis: what arrangements were actually proposed and rejected at each decision point (1920s, 1930s, 1940s)? Were alternative paths (labor-zionist gradualism, political-zionist negotiated partition, international mandate) objectively foreclosed, or were they deliberately rejected by the revisionist movement? Do other Zionist readings (labor, political, cultural) offer viable alternatives?',
    'If the Iron Wall doctrine was chosen among available alternatives, the constraint is an ideological choice, not a necessary structural response — extraction becomes the primary function rather than a necessary adjunct to solving the Jewish Question. If foreclosed objectively, the high suppression is justified by circumstances beyond the constraint''s authorship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iron_wall_doctrine_necessity, conceptual, 'Whether the Iron Wall mechanism is structurally necessary or an ideological choice among alternatives.').

omega_variable(
    arab_resistance_endogeneity,
    'Is Arab rejection and military resistance a given to which the Iron Wall doctrine is a response, or is Arab rejection itself partly produced by the territorial claim and the enforcement mechanism, creating the very resistance the constraint claims to address?',
    'Historical analysis of Arab political positions pre- and post-territorial claim assertion: did Arab actors initially accept Jewish settlement and only resist after maximalist claims? Or was the rejection of territorial displacement inevitable from the outset? Comparative analysis with other settler-colonial projects: does resistance emerge predictably when territorial claims are asserted and enforced, suggesting the resistance is endogenous to the constraint rather than exogenous to it?',
    'If Arab resistance is endogenous (produced by the constraint), the Iron Wall doctrine is circularly justified — the constraint creates resistance it then claims to suppress. If exogenous (independent Arab rejection), the enforcement is responsive to genuine opposition. The distinction reshapes the constraint''s classification: endogenous resistance suggests pure extraction dressed as necessary defense; exogenous resistance suggests legitimate (if asymmetric) contestation of a territorial claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_resistance_endogeneity, empirical, 'Whether Arab military resistance is given or produced by the constraint''s operation.').

omega_variable(
    reading_vs_reality_gap,
    'Is the revisionist-Zionism reading''s account of what the constraint accomplishes (Jewish security, national dignity, resolution of the Jewish Question) empirically supported, or does the constraint''s actual operation diverge from the reading''s justification?',
    'Measurement of Jewish security outcomes over the interval and post-1948: do security metrics improve with the constraint''s enforcement, or do they improve via other mechanisms (international recognition, state-building, institutional integration)? Analysis of whether the constraint''s operation produces the claimed dignity (agency, self-determination, national pride) or produces correlated phenomena (trauma from displacement and military enforcement, cognitive dissonance from contradiction with claimed Jewish ethical tradition, institutional capture by military and security apparatus).',
    'If the reading''s justifications are not borne out, the constraint becomes a case of false-summit mountain: presented as natural necessity but maintained by beneficiary extraction. If justified, the asymmetry in beneficiary/victim distribution remains but the constraint''s function is clarified as a genuine (if unjust) coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_reality_gap, empirical, 'Degree of correspondence between the revisionist reading''s normative claims and the constraint''s empirical outcomes.').

omega_variable(
    kernel_contest_structural_location,
    'What is the structural locus of the disagreement among the four Zionist readings? Is it located in differing axioms about Jewish national essence, differing empirical beliefs about what produces Jewish security, differing moral frameworks about the permissibility of Arab displacement, or differing strategic judgments about which path is politically viable?',
    'Textual and intellectual history analysis of the foundational writings of each reading: Jabotinsky (revisionist), Borochov and Ben-Gurion (labor), Herzl (political), Ahad Ha''am (cultural). Identify where each reading''s axioms diverge: on the necessity of Arab consent, on the permissibility of displacement, on the role of labor/socialism, on the sufficiency of cultural/spiritual center without political sovereignty.',
    'If the disagreement is purely empirical (differing beliefs about security outcomes), the readings potentially converge as evidence accumulates. If axiomatically grounded (differing views on Jewish national essence and permissible methods), the readings remain in permanent coexistence. If strategically located (differing views on political viability and international support), the readings'' relative power shifts with geopolitical context. The distinction determines whether the kernel is tractable to resolution or structurally contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_structural_location, conceptual, 'The structural location of disagreement among Zionist readings of the territorial claim kernel.').

omega_variable(
    settler_internalization_vs_structural_suppression,
    'Is the suppression maintaining Palestinian acceptance of Jewish territorial claims primarily structural (military force, administrative control, denial of voice) or partially internalized (Palestinians'' own beliefs about the inevitability of Jewish sovereignty, absorbed from education systems and dominant narratives)?',
    'Post-suppression-lift observation: when Israeli military control is reduced or withdrawn (e.g., in negotiated settlement scenarios or de facto Palestinian autonomy), does Palestinian political organization and territorial assertion re-emerge immediately, suggesting suppression was primarily structural, or do Palestinian populations continue to defer to Israeli claims, suggesting internalization? Comparative analysis: Palestinian political movements that operate outside direct Israeli control (diaspora, international forums) — do they articulate independent territorial claims, suggesting internalization is not complete?',
    'If primarily structural, lifting suppression would allow Palestinian agency to re-emerge, and the constraint''s persistence would depend on continued military enforcement. If substantially internalized, even lifting suppression would not restore Palestinian voice, and the constraint''s operation would shift from coercive to hegemonic (maintaining psychological consent rather than physical force).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_internalization_vs_structural_suppression, empirical, 'The balance of structural vs. internalized suppression in Palestinian acceptance of the territorial claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(jewi_tr_t1925, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1925, 0.11).
narrative_ontology:measurement(jewi_tr_t1930, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1930, 0.14).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1936, 0.17).
narrative_ontology:measurement(jewi_tr_t1942, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1942, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.22).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1920, 0.62).
narrative_ontology:measurement(jewi_be_t1925, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1925, 0.68).
narrative_ontology:measurement(jewi_be_t1930, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1930, 0.75).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1936, 0.81).
narrative_ontology:measurement(jewi_be_t1942, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1942, 0.84).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement(jewi_su_t1925, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1925, 0.68).
narrative_ontology:measurement(jewi_su_t1930, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1930, 0.75).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1936, 0.82).
narrative_ontology:measurement(jewi_su_t1942, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1942, 0.88).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__revisionist_zionism_reading, 0.25).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, palestinian_national_self_determination).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jordanian_state_sovereignty).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, arab_rejection_of_partition).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested kernel jewish_territorial_claim. The kernel represents a stabilized commitment (Jewish people have some territorial/political interest in Palestine) that different Zionist movements read as structurally different constraints. This reading (revisionist_zionism_reading) asserts territorial maximalism and military enforcement; the sibling readings (political_zionism, labor_zionism, cultural_zionism) instantiate alternative constraints with different ε values, different beneficiary/victim structures, and different enforcement mechanisms. The network links all four readings to their sibling readings and to downstream constraints on Palestinian self-determination, Jordanian sovereignty, and Arab collective responses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
