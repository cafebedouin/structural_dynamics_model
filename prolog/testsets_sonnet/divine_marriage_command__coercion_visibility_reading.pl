% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: 1890 Manifesto as Coercion-Acknowledged Institutional Survival Reading
 *   domain: religious_authority/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the coercion_visibility_reading of the
 *   divine_marriage_command kernel: the 1890 Manifesto ending the public
 *   practice of plural marriage is read as an acknowledged institutional
 *   response to sustained federal coercion (escheatment, disenfranchisement,
 *   imprisonment), with the institution's later theological legitimacy for
 *   the change resting substantially on the fact of institutional survival
 *   rather than on a clean revelatory event. This is one of three readings of
 *   the same kernel. The continuationist_reading holds the command was never
 *   doctrinally rescinded, only prudentially suspended under duress; the
 *   substitutionist_reading holds the Manifesto is itself new revelation that
 *   superseded the prior command outright. This story does not adjudicate
 *   between them — it generates only the coercion-visibility claim as its own
 *   ε-invariant constraint, per Rule 1.
 *
 * KEY AGENTS:
 *   - church_institutional_leadership: agenda_setter/beneficiary (institutional/arbitrage) — administers the reading, controls its later characterization
 *   - federal_government_authority: beneficiary (institutional/analytical) — achieved its coercive policy goal and set the precedent
 *   - plural_wives_and_children_post_manifesto: payer (powerless/trapped) — bear the cost of institutional ambiguity
 *   - continuationist_dissenting_members: payer (moderate/constrained) — punished for taking the prior revelatory claim at face value
 *   - excommunicated_fundamentalist_offshoots: excluded (powerless/trapped) — severed from the doctrinal conversation entirely
 *   - church_historians_and_outside_scholars: observer (analytical/analytical) — hold the documentary record this reading depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.58).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.62).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "1890 Manifesto as Coercion-Acknowledged Institutional Survival Reading").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, 'e2832d84-1e70-4e5e-a24e-a08a591a66e2').
narrative_ontology:cs_kernel_codification('e2832d84-1e70-4e5e-a24e-a08a591a66e2', fixed_text).
narrative_ontology:cs_authority_grounding('e2832d84-1e70-4e5e-a24e-a08a591a66e2', lineage).
narrative_ontology:cs_interpretation_layer_present('e2832d84-1e70-4e5e-a24e-a08a591a66e2').
narrative_ontology:cs_reading_relation('e2832d84-1e70-4e5e-a24e-a08a591a66e2', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2832d84-1e70-4e5e-a24e-a08a591a66e2', divine_marriage_command__substitutionist_reading, influences).
narrative_ontology:cs_axiom('e2832d84-1e70-4e5e-a24e-a08a591a66e2', foundational, institutional_survival_as_theological_warrant).
narrative_ontology:cs_axiom_status(institutional_survival_as_theological_warrant, holdable).
narrative_ontology:cs_axiom_grounding('e2832d84-1e70-4e5e-a24e-a08a591a66e2', institutional_survival_as_theological_warrant, instrumental).
narrative_ontology:cs_axiom('e2832d84-1e70-4e5e-a24e-a08a591a66e2', secondary, exogenous_coercion_admissible_as_revelatory_occasion).
narrative_ontology:cs_axiom_status(exogenous_coercion_admissible_as_revelatory_occasion, holdable).
narrative_ontology:cs_axiom_grounding('e2832d84-1e70-4e5e-a24e-a08a591a66e2', exogenous_coercion_admissible_as_revelatory_occasion, conventional).
narrative_ontology:cs_reference_frame('e2832d84-1e70-4e5e-a24e-a08a591a66e2', revealed_command_as_binding_covenant).
narrative_ontology:cs_drift_state('e2832d84-1e70-4e5e-a24e-a08a591a66e2', post_manifesto_institutional_narrative_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e2832d84-1e70-4e5e-a24e-a08a591a66e2', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, post_manifesto_membership_seeking_civic_legitimacy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, federal_government_authority).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, plural_wives_and_children_post_manifesto).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, continuationist_dissenting_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, excommunicated_fundamentalist_offshoots).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, institutional_survival_as_theological_warrant).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, federal_supremacy_over_religious_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and administers the Manifesto, framing it publicly as the President's counsel and as compliance with civil law while privately and later publicly navigating whether it constitutes a change in doctrine or a suspension under duress. Retains control over the institution's continued corporate existence, temple property, and political rehabilitation. Can revise the official narrative over time (from 'temporary accommodation' to 'inspired revelation') as institutional needs shift.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, church_institutional_leadership, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, church_institutional_leadership, beneficiary).

% Applied escheatment threats, disenfranchisement, and imprisonment (Edmunds-Tucker Act, Late Corporation dissolution) to force the change. Achieves its policy goal — abolition of plural marriage as a visible practice — and gains a durable precedent for federal authority to override a religious body's professed revelatory practice through economic and political coercion.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government_authority, beneficiary,
    institutional, generational, analytical, national).

% Existing plural families are told the new marriages must stop, but they are not simply dissolved; wives and children live in legal and social limbo, often denied full legitimacy, inheritance clarity, or public acknowledgment, while the institution that created their family structure now distances itself from having sanctioned it. They cannot exit the consequences of a covenant they entered in good faith under the institution's prior authority.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, plural_wives_and_children_post_manifesto, payer,
    powerless, biographical, trapped, local).

% Members who hold that plural marriage was never doctrinally rescinded, only prudentially suspended, are placed in direct conflict with an institution that (under this reading) treats the change as legitimated by survival necessity rather than revelation of equal authority. Continuing the practice risks excommunication; abandoning it risks what they see as covenant violation. Their exit options are limited to quiet non-compliance, migration to schismatic groups, or submission.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, continuationist_dissenting_members, payer,
    moderate, biographical, constrained, regional).

% Groups that continued the practice after 1890 (and after the 1904 Second Manifesto) were formally severed from the institution and subjected to state raids and prosecution decades later. They would argue the coercion-visibility reading is correct as description but wrong as theology — that survival necessity cannot supersede revealed command — but they are not part of the institution's ongoing doctrinal conversation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, excommunicated_fundamentalist_offshoots, excluded,
    powerless, generational, trapped, regional).

% The broader lay membership, especially in Utah and the Mountain West, benefits from statehood, restored voting and property rights, and reduced federal hostility once the practice visibly ends. They gain civic normalization and economic integration at the cost of institutional silence about how and why the change actually happened.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, post_manifesto_membership_seeking_civic_legitimacy, beneficiary,
    organized, generational, mobile, national).

% Document the documentary record — the Woodruff journal entries, the timing relative to federal legislation, the later Second Manifesto, and the institution's shifting official characterizations across decades. They are not party to the institution's legitimacy claims but their scholarship is the primary evidence any outside party has for adjudicating between readings.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, church_historians_and_outside_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, church_institutional_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution a mechanism to preserve its corporate existence, property, and members' civil rights by aligning publicly observable practice with federal law, without requiring the institution to explicitly repudiate the prior revelatory claim in a single stroke.
% TRANSFER_FUNCTION: Moves civic legitimacy, property protection, and political rehabilitation to the institution and its mainstream membership; moves the cost of ambiguity and abandonment onto existing plural families and onto those who took the original revelatory claim at face value and continued the practice.
% ABSENT_VOICES: The plural wives themselves were not authors of the Manifesto and are rarely named as parties to its negotiation; fundamentalist offshoot groups that trace continuous authority from the original revelation are excommunicated and structurally excluded from the institution's later doctrinal conversation about what the Manifesto actually meant.
% DISAPPEARANCE_RATIONALE: If this reading were dropped — if the institution stopped being able to point to documented federal coercion as part of the Manifesto's context — the entire legitimating apparatus for treating institutional survival as a valid theological input would collapse, forcing a direct choice between the continuationist and substitutionist readings and reopening litigated questions about the status of post-Manifesto plural families and excommunicated offshoots.
% FOUNDING_PROBLEM: The federal government's escalating legal and economic pressure (disenfranchisement, property seizure, imprisonment of practitioners) threatened the institution's corporate survival; the Manifesto was issued to relieve that pressure by ending the practice's visibility.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative history (Edmunds-Tucker Act, the Late Corporation of the Church of Jesus Christ of Latter-Day Saints v. United States decision) and independent historians outside the institution corroborate that the coercion was real, severe, and temporally proximate to the Manifesto's issuance; the acute federal threat itself no longer exists, though the institution's own retrospective characterizations of the Manifesto's theological status have shifted across the twentieth century without ever centering the coercion as the operative cause.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the reading itself extracts institutional benefit (survival, statehood, rehabilitation) at the direct cost of plural families and dissenters, but it is not maximal because the coordination function (avoiding institutional dissolution) is genuine and shared broadly across ordinary membership. Suppression peaks sharply in 1887-1890 (federal legal machinery at its most active) and declines steadily afterward as the practice's visibility recedes and legal pressure eases — this is the coercion apparatus itself, not the reading's own enforcement. Theater ratio rises steeply and monotonically: as the acute coercion recedes into history, the institution's public discourse about the Manifesto increasingly performs continuity and inspired guidance rather than directly naming the federal pressure that this reading holds as operative — hence the widening gap between actual cause (theater_ratio trending toward 0.71) and public theological framing.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's seat, the coercion-visibility reading is an uncomfortable but survivable acknowledgment folded into a larger inspired-guidance narrative — d sits near the beneficiary end because the institution retained corporate continuity and eventual political rehabilitation. From the plural wives' and continuationist dissenters' seats, this same reading exposes that their covenant relationships were sacrificed to institutional self-preservation rather than treated as the direct object of new revelation — d sits near the full-target end. The federal government's seat experiences the constraint purely as successful policy outcome, outside the theological stakes entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institutional leadership and the federal government are coded as beneficiaries: the institution gains survival and eventual statehood/rehabilitation, and the federal government gains a durable precedent and achieves its coercive policy objective. Plural wives, their children, and continuationist dissenters are coded as victims: their family and covenant relationships bear the cost of an ambiguous, survival-driven doctrinal shift they did not initiate and could not exit without severe personal cost. Fundamentalist offshoots are excluded rather than merely victimized — they are structurally removed from the conversation that produced this reading, which is why they carry `excluded` rather than `payer` as primary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (imminent federal dissolution of the institution) is dead — no comparable federal threat exists today — yet the coercion-visibility framing persists as a live historical-theological argument, primarily wielded by outside historians and dissenting factions rather than by the institution itself, which has largely moved toward a substitutionist-adjacent public narrative over the twentieth century. This is a mismatch worth flagging: founding_problem_status=dead paired with a still-contested disappearance_verdict=world_rearranges signals that the reading's persistence is now driven by its evidentiary and legitimacy stakes for present-day dissenting and offshoot groups, not by any live coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_as_valid_theological_input,
    'Can documented external coercion ever constitute a legitimate ground for a change in revealed doctrine, or does acknowledging coercion as operative necessarily reduce the change to pure political capitulation with no independent theological standing?',
    'This is not resolvable by additional historical evidence — the documentary record of federal pressure is already extensive and largely uncontested. Resolution would require the institution itself to take an explicit position on whether survival-necessity can ground revelation, which it has structurally avoided doing for over a century.',
    'If coercion is accepted as a valid theological input, this reading approaches a stable tangled_rope (real coordination benefit for institutional survival, real extraction from those whose covenants were left ambiguous). If coercion is rejected as valid input, this reading collapses toward the continuationist reading''s implicit charge that the Manifesto has no independent doctrinal standing at all, which would push it toward snare from the continuationist and offshoot seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_as_valid_theological_input, conceptual, 'Whether institutional survival can validly ground doctrinal legitimacy under this reading.').

omega_variable(
    sibling_reading_divergence_location,
    'Where exactly do the three kernel readings diverge — is it the causal claim (did coercion operate), the theological claim (does coercion void or ground legitimacy), or the retrospective claim (what does the institution now say happened)?',
    'Careful separation of the documentary/causal question (largely settled among historians) from the theological warrant question (contested, values-driven) and the institutional self-narrative question (empirically trackable via official statements over time, e.g. the shift from 1890 rhetoric to later characterizations).',
    'If the divergence is purely theological (as this omega suggests), then all three readings can share the same causal/historical ε-inputs while producing different classifications purely from differing beneficiary/victim and legitimacy structures — which is exactly the decomposition this story attempts. Misidentifying the divergence location risks conflating causal disagreement with theological disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_divergence_location, conceptual, 'Precise location of structural disagreement among the three kernel readings.').

omega_variable(
    second_manifesto_status,
    'Does the 1904 Second Manifesto (which added explicit excommunication penalties) represent reinforcement of this coercion-visibility reading, a shift toward substitutionist self-understanding, or an independent event requiring its own constraint story?',
    'Compare the stated rationale and enforcement mechanism of the 1904 Second Manifesto against the 1890 original — if its rationale differs materially (e.g. explicit doctrinal claims not present in 1890), the ε-invariance principle would require decomposing it into a separate linked constraint rather than treating it as a mere continuation of this one.',
    'If the Second Manifesto rests on materially different grounds, this story''s interval and measurement series (which currently span through 1904 without a break) may be conflating two distinct constraints under one continuous timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_manifesto_status, empirical, 'Whether the 1904 Second Manifesto is the same constraint continued or a distinct linked constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 1862, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1862, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement(divi_tr_t1887, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1887, 0.2).
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(divi_tr_t1904, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1904, 0.55).
narrative_ontology:measurement(divi_tr_t1953, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1953, 0.65).
narrative_ontology:measurement(divi_tr_t1990, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1990, 0.7).
narrative_ontology:measurement(divi_tr_t2020, divine_marriage_command__coercion_visibility_reading, theater_ratio, 2020, 0.71).

% Extraction over time
narrative_ontology:measurement(divi_be_t1862, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1862, 0.2).
narrative_ontology:measurement(divi_be_t1887, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1887, 0.35).
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(divi_be_t1904, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1904, 0.6).
narrative_ontology:measurement(divi_be_t1953, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1953, 0.58).
narrative_ontology:measurement(divi_be_t1990, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1990, 0.56).
narrative_ontology:measurement(divi_be_t2020, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 2020, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1862, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1862, 0.3).
narrative_ontology:measurement(divi_su_t1887, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1887, 0.75).
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1890, 0.68).
narrative_ontology:measurement(divi_su_t1904, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1904, 0.7).
narrative_ontology:measurement(divi_su_t1953, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1953, 0.5).
narrative_ontology:measurement(divi_su_t1990, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(divi_su_t2020, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 2020, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the 1890 Manifesto' / 'the divine marriage command kernel' per the ε-invariance principle. The continuationist_reading and substitutionist_reading are separate constraint stories, each with its own ε, beneficiary/victim structure, and classification, linked here via affects_constraints. This reading (coercion_visibility_reading) shares the historical/causal record with both siblings but diverges from them on the theological warrant question — whether documented federal coercion can ground doctrinal legitimacy (this reading), whether it merely suspends without rescinding (continuationist), or whether it is subordinated to an independent revelatory claim (substitutionist).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
