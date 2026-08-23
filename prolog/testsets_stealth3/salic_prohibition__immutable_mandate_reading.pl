% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Agnatic Succession Prohibition as Irrevocable Natural/Divine Law (Immutable Mandate Reading of the Salic Prohibition)
 *   domain: constitutional/dynastic/political-history
 *
 * SUMMARY:
 *   The arrangement under contest is the agnatic succession prohibition as
 *   the immutable mandate reading holds it: not a statute a sovereign could
 *   revise, but a natural and divine ordinance embedded in the dynastic
 *   constitution itself, binding kings and realms alike. Articulated in the
 *   succession determinations of 1316–1328, enforced by a century of war
 *   against the female-line claim of Edward III, systematized by the
 *   Parlement of Paris into the doctrine of the kingdom's fundamental laws,
 *   raised to its apex under divine-right monarchy, and left after 1789 as
 *   the legitimist remnant's de jure claim, the reading's own doctrine
 *   authorizes its distinctive enforcement posture: challengers to a female
 *   succession act legitimately, and preventive war in defense of agnatic
 *   priority is just. This file instantiates ONE reading of the
 *   salic_prohibition kernel; the sibling readings are separate constraints
 *   linked through network.affects_constraints. Authoring is layered and the
 *   layers are kept independent: the claimed type and the metric values are
 *   authored from the reading's own seat (epsilon is reading-indexed over the
 *   fixed referent of the standing arrangement); the beneficiary, victim, and
 *   enforcement declarations are asserted as structural facts for the
 *   engine's directionality derivation, and the reading itself would not
 *   frame its world in those terms — that divergence is precisely the
 *   false-summit question the omega variables carry and the FSM machinery
 *   exists to evaluate. KEY AGENTS (by structural relationship): see
 *   key_agents; the analytical observer seat (constitutional_historians) sees
 *   the full structure across all historical seats.
 *
 * KEY AGENTS:
 *   - reigning_agnatic_monarch: agenda-setting beneficiary (institutional / identity_locked) — holds the crown under the rule, proclaims and enforces it, cannot exit his own constitution without dissolving his legitimacy
 *   - princes_of_the_blood: primary beneficiary (powerful / constrained) — male collaterals elevated by the exclusion of female lines; the seat the crown's gains accrue to across successions
 *   - canonist_clergy: secondary beneficiary (organized / constrained) — supplies the divine-law framing and collects interpretive authority from it
 *   - royal_parlement_of_paris: co-agenda-setter (institutional / constrained) — registers, adjudicates, and defends the rule as fundamental law
 *   - excluded_female_line_claimants: primary target (powerless / trapped) — daughters and their descendants categorically barred, with no court in which they themselves may be heard
 *   - succession_war_populations: diffuse target (powerless / trapped) — bear the enforcement wars' costs without having chosen the quarrel
 *   - queen_mothers_and_regents: excluded voice (powerful / constrained) — govern in practice, barred in principle; would object but are not in the succession conversation
 *   - constitutional_historians: analytical observer — sees the ad hoc origins, the retroactive systematization, and the divergence between self-presentation and operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.15).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.15).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, mountain).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Agnatic Succession Prohibition as Irrevocable Natural/Divine Law (Immutable Mandate Reading of the Salic Prohibition)").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional/dynastic/political-history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).
domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'ad98d200-202c-4e2f-8897-4f7869481f9f').
narrative_ontology:cs_kernel_codification('ad98d200-202c-4e2f-8897-4f7869481f9f', fixed_text).
narrative_ontology:cs_authority_grounding('ad98d200-202c-4e2f-8897-4f7869481f9f', lineage).
narrative_ontology:cs_interpretation_layer_present('ad98d200-202c-4e2f-8897-4f7869481f9f').
narrative_ontology:cs_reading_relation('ad98d200-202c-4e2f-8897-4f7869481f9f', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('ad98d200-202c-4e2f-8897-4f7869481f9f', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('ad98d200-202c-4e2f-8897-4f7869481f9f', foundational, agnatic_exclusion_irrevocable_divine_law).
narrative_ontology:cs_axiom_status(agnatic_exclusion_irrevocable_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('ad98d200-202c-4e2f-8897-4f7869481f9f', agnatic_exclusion_irrevocable_divine_law, theological).
narrative_ontology:cs_axiom('ad98d200-202c-4e2f-8897-4f7869481f9f', secondary, preventive_war_justifiable_for_agnatic_priority).
narrative_ontology:cs_axiom_status(preventive_war_justifiable_for_agnatic_priority, holdable).
narrative_ontology:cs_axiom_grounding('ad98d200-202c-4e2f-8897-4f7869481f9f', preventive_war_justifiable_for_agnatic_priority, instrumental).
narrative_ontology:cs_reference_frame('ad98d200-202c-4e2f-8897-4f7869481f9f', sacral_agnatic_fundamental_law).
narrative_ontology:cs_drift_state('ad98d200-202c-4e2f-8897-4f7869481f9f', contemporary, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ad98d200-202c-4e2f-8897-4f7869481f9f', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, reigning_agnatic_monarch).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, princes_of_the_blood).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, canonist_clergy).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, excluded_female_line_claimants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, succession_war_populations).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, agnatic_priority_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, fundamental_laws_irrevocability).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, sacral_kingship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the crown under a rule he did not make and cannot revise: the succession law that put him there also binds him, and his legitimacy rests on proclaiming it divine and unchangeable. He directs the rule's enforcement — declaring female-line challengers usurpers, commissioning the juridical defenses, and where needed waging war against rival claimants. His identity as king is constituted by the rule's authority; abandoning it would dissolve the ground he stands on.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, reigning_agnatic_monarch, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, reigning_agnatic_monarch, beneficiary).

% Male collaterals of the royal house — brothers, nephews, cousins in the male line — whose place in the order of succession exists only because female lines are excluded. Each king's death moves them up the list; the rule converts distant kin into heirs apparent and appanage holders. They defend the rule in council and in arms, since any relaxation would demote them behind the daughters and granddaughters of kings.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, princes_of_the_blood, beneficiary,
    powerful, generational, constrained, national).

% The theologians and canon lawyers who supply the divine-law framing: they preach the sacral character of the succession rule, staff the councils that declare it fundamental, and hold interpretive authority over what God's ordinance requires. Their place in the constitutional order — and their leverage over the crown — depends on the rule being a matter of divine law rather than mere statute.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, canonist_clergy, beneficiary,
    organized, generational, constrained, continental).

% The sovereign court that registers and adjudicates the succession rule: it heard the 1316–1328 determinations, wove them into the doctrine of the kingdom's fundamental laws, and later defended that doctrine even against treaties the crown had signed. It administers the rule's application at each succession and can make its defense a condition of registering royal acts.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, royal_parlement_of_paris, agenda_setter,
    institutional, generational, constrained, national).

% Daughters, granddaughters, and their descendants in the line of succession: Joan of Navarre, the daughters of Philip V and Charles IV, Isabella of France and her son Edward III. The rule removes them from the succession entirely and gives their claims no court in which they themselves may be heard — a woman cannot transmit a right she cannot hold, so their claims survive only where a male champion chooses to prosecute them. They bear the exclusion directly; where their champions press it, they bear a war.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, excluded_female_line_claimants, payer,
    powerless, generational, trapped, continental).

% The peasants, townspeople, and soldiery of France and England who supply the taxes, provisions, and bodies that enforcing the succession rule consumes. They had no part in declaring the fundamental law and no exit from its enforcement: the chevauchees, the sieges, and the decades of campaigning of the Hundred Years' War fell on their fields and their sons regardless of which claimant they might have preferred.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, succession_war_populations, payer,
    powerless, immediate, trapped, regional).

% Widowed queens who governed realms in minority regencies — exercising royal power in practice while the succession rule barred them from the throne in principle. They negotiated treaties, led councils, and raised armies, yet had no standing in the succession adjudications that disposed of their own children's claims; they would object that the rule treats as incapable the very women it relies on to govern.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, queen_mothers_and_regents, excluded,
    powerful, biographical, constrained, national).

% Modern scholars of dynastic law who can see the whole structure at once: the ad hoc character of the 1316–1328 determinations, the retroactive systematization of the 'fundamental laws,' the spurious textual anchor in the Frankish code, and the divergence between the rule's self-presentation and its operation. They take testimony from all the historical seats and owe nothing to any of them.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, princes_of_the_blood).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single determinate answer to 'who reigns next' at every succession: the crown descends exclusively through male lines in strict agnatic order, so the realm avoids partition, election contests, and acquisition of the throne by foreign houses through marriage lines. The rule also stabilizes the dynasty as a sacred corporate body whose continuity is not renegotiated at each king's death.
% TRANSFER_FUNCTION: Moves succession rights — and with them the crown, domain revenues, and war-making authority — from female heirs and their descendants to male agnatic collaterals; and moves the costs of enforcing that allocation (campaigning, devastation, war taxation) onto the populations of the contending kingdoms.
% ABSENT_VOICES: The women whose claims were adjudicated — Joan of Navarre, the daughters and granddaughters of kings — had no standing to speak in their own successions; their cases were argued by male guardians or abandoned when male protectors withdrew. The populations who would fight the enforcement wars were absent from the councils that declared the fundamental law. Queen mothers who exercised regnal power were consulted on everything except the rule that barred them.
% DISAPPEARANCE_RATIONALE: If the immutable mandate vanished overnight, succession would follow whatever cognatic, elective, or testamentary principle each realm's next-strongest party could press: the Navarrese crown would have gone to Joan in 1316 without litigation; Edward III's claim through Isabella would not have been barred, and the Hundred Years' War's central casus belli dissolves; later realms would have revised succession by instrument (as Spain did) rather than by war. The dynastic map of Europe — Valois France, the Lancastrian and Yorkist contests, the Spanish successions — reorganizes around whichever allocation each realm's living claimants could enforce.
% FOUNDING_PROBLEM: The Capetian succession crisis of 1316–1328: three consecutive kings died leaving only daughters or no sons (Louis X leaving Joan, John I's ten-day life, Charles IV leaving daughters), the direct male line faced extinction, and the realm needed a rule that would keep the crown out of foreign hands (Edward III through Isabella) and out of partition among claimants.
% FOUNDING_PROBLEM_CORROBORATION: The founding crisis is corroborated by sources outside the benefiting parties: the surviving determinations of 1316–1328, the Parlement record, the contemporaneous chronicles, and the Navarrese party's own protests over Joan's exclusion. That the founding problem is now dead is attested by the same record's continuation: the direct Capetian line, the contested successions, and hereditary monarchy itself have all passed away, and no party outside legitimist remnant circles attests that the problem the rule was built for still exists.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All metric values are authored from the reading's own seat: the immutable mandate reading assesses the agnatic prohibition as constitutive divine order, not as taking — in its lights women hold no succession right that could be taken, male agnates receive their own by ordinance, and the force it authorizes (war against challengers) is defense of order, not coercion of a rightful claim. Hence the low extractiveness (0.15 at interval end), low suppression (0.15), and low theater (0.20 — the reading takes its sacral apparatus at face value, though even its lights register that a coronation cannot be performed without a throne). Accessibility collapse is high (0.85) because a natural law admits no alternative in kind; resistance is low (0.20) because what the reading's lights classify as resistance is rebellion, and in its self-account the rebellion failed. The temporal series share one grid (T=0 = 1316, unit ~11 years, T=60 ~ 1976; base_properties carry the T=60 end-state). The suppression_requirement series is the story's live dynamic and the one quantity the reading's own doctrine tracks honestly: enforcement built from juridical determination (0.30) through a century of authorized war (0.60), settled into doctrinal defense of the fundamental laws (0.50–0.55), and collapsed with the monarchy (0.15). The acknowledged-extraction series stays flat while enforcement escalates and then dies — escalating coercion against flat acknowledged extraction is the naturalization signature this story exists to record; an analyst-lights extraction series would rise as the doctrine hardened and expanded, and that divergence is carried by the structural declarations and the omegas rather than by the reading's own series.
 *
 * PERSPECTIVAL GAP:
 *   From the monarch's seat the arrangement is the divine constitution he embodies: he did not make the rule and cannot revise it, and his identity is constituted by proclaiming it. From the princes' seat it is the ladder each king's death lets them climb. From the canonists' seat it is the source of their leverage over the crown. From the female-line seats it is a categorical dispossession with no court in which the dispossessed may be heard — their only leverage ever came through coalition with powerful male champions (Edward III being the paradigm), which is itself a measure of how the seat is structured. From the war populations' seat it is a tax in blood levied for a quarrel about descent they did not choose and could not exit. The engine computes per-seat classifications from the power, exit, and role data; the reading's authored claim adjudicates none of these divergences, and the gap between the agenda-setting seats and the payer seats is the perspectival content this story contributes.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared beneficiaries — the reigning monarch, the princes of the blood, and the canonist clergy — sit near the beneficiary end of directionality: the rule subsidizes them with the crown, with heir-apparency and appanage, and with interpretive authority over the divine-law framing. The declared victims — the excluded female-line claimants and the succession-war populations — sit near the target end: categorical dispossession with no legal avenue (trapped exit), and enforced war costs with no exit at all. The queen-mother/regent seat is genuinely dual — subsidized by regency practice, targeted by the succession bar — and carries no beneficiary or victim declaration, so its directionality rides the canonical fallback for its power atom; that residual is acknowledged rather than forced with an override, since an override keyed to its power atom would also capture the princes' seat. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is what the engine scales by directionality and spatial scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview locates the question precisely: the founding problem — deciding the 1316–1328 successions without partition or foreign acquisition — is dead, corroborated as dead by the same record that corroborates the crisis itself, while the world still rearranges around the rule in one respect: legitimist remnant communities still organize identity and de jure claims by it. The dead-problem × world-rearranges mismatch is the capture/zombie signature: the reading persists as dynastic identity after its function and its enforcement capacity both lapsed. The classification apparatus prevents two opposite mislabelings here: it prevents reading the arrangement as pure coordination (the succession-determinacy function is real, but the same structure categorically dispossesses female lines and legitimizes war), and it prevents treating the mountain claim as settled (the natural-law-versus-constructed omega keeps the false-summit question open for the FSM machinery, which exists for exactly this shape — a natural-law claim over an allocation with identifiable beneficiaries). If the engine reclassifies via the false-summit signature, that reclassification is the measurement, not an error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_allocation,
    'Is the agnatic prohibition a genuine natural or divine law embedded in the dynastic constitution, as this reading claims, or a constructed allocation — articulated in the 1316–1328 determinations to resolve the Capetian crisis, benefiting identifiable male-line parties, and retroactively naturalized as ''fundamental law''?',
    'Genealogical and juridical history: whether any agnatic succession doctrine predates the 1316 determinations, whether the Frankish text invoked as anchor concerns private land rather than the crown, and whether the ''fundamental laws'' systematization postdates the determinations it claims to express.',
    'If constructed, the mountain claim collapses and the arrangement computes as a tangled rope — real coordination (succession determinacy) carrying asymmetric extraction (categorical dispossession of female lines) under active enforcement; if genuine natural law, the mountain classification stands and the enforcement doctrine is defense of order rather than maintenance of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_allocation, empirical, 'Whether the immutable-mandate claim reflects natural law or retroactive naturalization of a political allocation.').

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the salic_prohibition kernel; the sovereign_override and cognatic_reversion readings instantiate different constraints from the same dynastic material — where exactly does the contest bite?',
    'Classify all three readings and compare their structural data: the disagreement is located in revocability (irrevocable ordinance versus sovereign-revisable statute) and in binding force (universal natural law versus territorially limited custom), not in the agnatic content itself; the sibling files carry their own epsilon, beneficiary, and victim structures.',
    'If the sovereign_override reading governs a realm (the Spanish pattern), revision happens by instrument and this reading''s enforcement posture loses its object; if the cognatic reading governs (the English and Navarrese pattern), the excluded class dissolves entirely and this reading''s victim set empties; if this reading governs, disputes resolve by war rather than legislation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: which reading of the salic_prohibition kernel governs, and what each sibling would change structurally.').

omega_variable(
    sacral_framing_sincerity,
    'Did the articulators and enforcers of the immutable mandate sincerely hold the natural/divine-law framing, or was it instrumental cover for an allocation they knew to be constructed?',
    'Private correspondence, council deliberations, and behavior under shifted incentives — whether the same actors treated parallel succession rules in other realms as equally divine, or as negotiable whenever the negotiable reading favored them.',
    'If instrumental, the theater_ratio is far higher than the reading''s self-assessment authors and the arrangement is extraction wearing natural-law dress; if sincere, the low authored theater stands and the false-summit question turns entirely on the constructed-versus-natural omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacral_framing_sincerity, empirical, 'Sincerity of the divine-law framing among the reading''s own articulators and enforcers.').

omega_variable(
    internalized_persistence_after_enforcement_collapse,
    'After the enforcement machinery collapsed (1789–1830), did the reading persist as internalized dynastic identity — legitimist self-concept carried without any capacity to enforce — rather than as structural coercion?',
    'Post-enforcement trajectory: whether legitimist communities maintained the exclusionary doctrine absent any enforcement capacity, and whether the doctrine dissolved when the dynastic identity that carried it dissolved.',
    'If internalized, the arrangement''s effective suppression outlives its structural enforcement and the remnant-era seats carry identity-locked dynamics the structural data alone would miss; if purely structural, suppression ended with the monarchy and the remnant is performance without force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_persistence_after_enforcement_collapse, empirical, 'Structural versus internalized persistence of the exclusion after enforcement collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_immutable_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(salic_immutable_tr_t0, observed).
narrative_ontology:measurement(salic_immutable_tr_t10, salic_prohibition__immutable_mandate_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(salic_immutable_tr_t10, observed).
narrative_ontology:measurement(salic_immutable_tr_t20, salic_prohibition__immutable_mandate_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(salic_immutable_tr_t20, observed).
narrative_ontology:measurement(salic_immutable_tr_t30, salic_prohibition__immutable_mandate_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(salic_immutable_tr_t30, observed).
narrative_ontology:measurement(salic_immutable_tr_t40, salic_prohibition__immutable_mandate_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(salic_immutable_tr_t40, observed).
narrative_ontology:measurement(salic_immutable_tr_t50, salic_prohibition__immutable_mandate_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(salic_immutable_tr_t50, observed).
narrative_ontology:measurement(salic_immutable_tr_t60, salic_prohibition__immutable_mandate_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(salic_immutable_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(salic_immutable_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(salic_immutable_be_t0, observed).
narrative_ontology:measurement(salic_immutable_be_t10, salic_prohibition__immutable_mandate_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(salic_immutable_be_t10, observed).
narrative_ontology:measurement(salic_immutable_be_t20, salic_prohibition__immutable_mandate_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(salic_immutable_be_t20, observed).
narrative_ontology:measurement(salic_immutable_be_t30, salic_prohibition__immutable_mandate_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement_basis(salic_immutable_be_t30, observed).
narrative_ontology:measurement(salic_immutable_be_t40, salic_prohibition__immutable_mandate_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement_basis(salic_immutable_be_t40, observed).
narrative_ontology:measurement(salic_immutable_be_t50, salic_prohibition__immutable_mandate_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement_basis(salic_immutable_be_t50, observed).
narrative_ontology:measurement(salic_immutable_be_t60, salic_prohibition__immutable_mandate_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement_basis(salic_immutable_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(salic_immutable_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(salic_immutable_su_t0, observed).
narrative_ontology:measurement(salic_immutable_su_t10, salic_prohibition__immutable_mandate_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(salic_immutable_su_t10, observed).
narrative_ontology:measurement(salic_immutable_su_t20, salic_prohibition__immutable_mandate_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(salic_immutable_su_t20, observed).
narrative_ontology:measurement(salic_immutable_su_t30, salic_prohibition__immutable_mandate_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(salic_immutable_su_t30, observed).
narrative_ontology:measurement(salic_immutable_su_t40, salic_prohibition__immutable_mandate_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(salic_immutable_su_t40, observed).
narrative_ontology:measurement(salic_immutable_su_t50, salic_prohibition__immutable_mandate_reading, suppression_requirement, 50, 0.18).
narrative_ontology:measurement_basis(salic_immutable_su_t50, observed).
narrative_ontology:measurement(salic_immutable_su_t60, salic_prohibition__immutable_mandate_reading, suppression_requirement, 60, 0.15).
narrative_ontology:measurement_basis(salic_immutable_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% One natural-language concept — 'Salic Law' — decomposes into three structurally distinct constraints by reading of the salic_prohibition kernel: this file (immutable mandate: irrevocable natural/divine law, enforcement-intensive, war-legitimizing), salic_prohibition__sovereign_override_reading (revocable positive law; the Spanish pattern of Auto Acordado and later Pragmatic Sanctions), and salic_prohibition__cognatic_reversion_reading (never properly binding; the English and Navarrese pattern). The readings share a referent (the agnatic prohibition) and author different epsilon by their own lights: this reading, as the arrangement's self-understanding, authors the lowest epsilon of the family; the cognatic reading sees an anachronistic imposition and will author the highest. Linked per the epsilon-invariance principle; the upstream immutable doctrine was historically cited as authority against both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
