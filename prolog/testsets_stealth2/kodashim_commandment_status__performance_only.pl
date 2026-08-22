% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Commandment-Status: Performance-Only Reading (Suspended Husk)
 *   domain: religious studies/halakhic theory/commitment system analysis
 *
 * SUMMARY:
 *   A halakhic community whose canon centers on a sacrificial code lost its
 *   performance site in 70 CE. Three readings of what the kodashim
 *   (sacrificial) commandments now demand compete inside the tradition. This
 *   file instantiates the performance_only reading: the commandment's force
 *   is wholly conditional on the altar; with the altar gone the commandment
 *   is suspended — a husk. The arrangement under contest is the standing one:
 *   the community's curriculum, ordination gates, funding claims, and
 *   practical-law discourse continue to treat kodashim as occupying
 *   commandment-status, drawing prime scholarly years and earmarked resources
 *   into material that, by this reading's lights, governs nothing
 *   performable. Per the epsilon-referent rule, epsilon is authored for that
 *   standing arrangement as this reading assesses it — never for the
 *   reading's endorsed alternative (reallocation of scholarly capacity to
 *   live law). Claim and metrics are independent authored facts: the claimed
 *   type states this reading's structural verdict; the metrics describe the
 *   arrangement's observed operation, and the engine measures any divergence.
 *   The kernel decomposes into three linked stories (this one,
 *   messianic_deferral, study_as_performance), each with its own epsilon,
 *   beneficiaries, and victims. KEY AGENTS (by structural relationship): -
 *   rabbinic_scholarly_establishment: Primary beneficiary and agenda-setter
 *   (institutional/identity_locked) — administers the curriculum, collects
 *   authority - yeshiva_institutions: Secondary beneficiary
 *   (organized/constrained) — collects funding keyed to total-corpus
 *   enrollment - yeshiva_students: Primary target (powerless/identity_locked)
 *   — bear the diverted years - lay_communities: Mixed payer-beneficiary
 *   (moderate/constrained) — fund the apparatus, receive continuity -
 *   halakhic_pragmatists: Excluded voice (moderate/mobile) — reallocation
 *   argument outside the conversation - academic_jewish_studies: Analytical
 *   observer (institutional/analytical) — documents the structure from
 *   outside
 *
 * KEY AGENTS:
 *   - rabbinic_scholarly_establishment: primary beneficiary/agenda-setter (institutional/identity_locked) — sets curriculum and examination gates, collects authority from total-corpus mastery
 *   - yeshiva_institutions: secondary beneficiary (organized/constrained) — collects tuition, donations, and state stipends keyed to full-time traditional enrollment
 *   - yeshiva_students: primary target (powerless/identity_locked) — spend prime years on suspended material; exit priced in identity
 *   - lay_communities: mixed payer-beneficiary (moderate/constrained) — fund the apparatus, receive continuity, bear opportunity cost
 *   - halakhic_pragmatists: excluded voice (moderate/mobile) — argue for reallocation to live law; no curricular consequence attaches to their case
 *   - academic_jewish_studies: analytical observer (institutional/analytical) — documents the migration of the sacrificial corpus from performed rite to curricular centerpiece
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.72).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.62).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.72).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Commandment-Status: Performance-Only Reading (Suspended Husk)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious studies/halakhic theory/commitment system analysis").

domain_priors:requires_active_enforcement(kodashim_commandment_status__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, 'ff348970-72a7-4b0a-a4c1-2104166d2a1c').
narrative_ontology:cs_kernel_codification('ff348970-72a7-4b0a-a4c1-2104166d2a1c', fixed_text).
narrative_ontology:cs_authority_grounding('ff348970-72a7-4b0a-a4c1-2104166d2a1c', lineage).
narrative_ontology:cs_interpretation_layer_present('ff348970-72a7-4b0a-a4c1-2104166d2a1c').
narrative_ontology:cs_reading_relation('ff348970-72a7-4b0a-a4c1-2104166d2a1c', kodashim_commandment_status__messianic_deferral, forecloses).
narrative_ontology:cs_reading_relation('ff348970-72a7-4b0a-a4c1-2104166d2a1c', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_axiom('ff348970-72a7-4b0a-a4c1-2104166d2a1c', foundational, commandment_force_wholly_altar_conditional).
narrative_ontology:cs_axiom_status(commandment_force_wholly_altar_conditional, holdable).
narrative_ontology:cs_axiom_grounding('ff348970-72a7-4b0a-a4c1-2104166d2a1c', commandment_force_wholly_altar_conditional, conventional).
narrative_ontology:cs_axiom('ff348970-72a7-4b0a-a4c1-2104166d2a1c', secondary, no_residue_demand_absent_performance_site).
narrative_ontology:cs_axiom_status(no_residue_demand_absent_performance_site, holdable).
narrative_ontology:cs_axiom_grounding('ff348970-72a7-4b0a-a4c1-2104166d2a1c', no_residue_demand_absent_performance_site, instrumental).
narrative_ontology:cs_reference_frame('ff348970-72a7-4b0a-a4c1-2104166d2a1c', performance_conditional_commandment_regime).
narrative_ontology:cs_drift_state('ff348970-72a7-4b0a-a4c1-2104166d2a1c', post_temple_curricular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff348970-72a7-4b0a-a4c1-2104166d2a1c', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, rabbinic_scholarly_establishment).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, yeshiva_institutions).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, yeshiva_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, lay_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, lay_communities).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, totality_of_torah_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, study_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the curriculum of the major academies, certifies deciders, and decides which parts of the canon carry binding weight in teaching and examination. Its standing rests on demonstrated mastery of the entire corpus, sacrificial orders included; conceding that a whole branch of the law governs nothing would unsettle the claim that its expertise spans the law as lived. Leaving that position would mean repudiating the foundation of its own authority.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, rabbinic_scholarly_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Collect tuition, donations, and government study stipends keyed to full-time enrollment in the traditional curriculum. Enrollment holds because families and donors want their sons formed in the whole Torah, hardest orders included. Reweighting the curriculum would mean recompeting for support against programs promising practical outcomes.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% Enter the track in late adolescence and spend their strongest years inside a fixed sequence of texts, a large share of which concerns sacrificial procedure nothing can perform. Community standing, matchmaking, and self-worth are bound up with progress through the whole sequence; stepping off is read as leaving the path, not picking a specialty.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_students, payer,
    powerless, immediate, identity_locked, regional).

% Fund the academies through donations and taxes and supply them with students. They receive continuity, identity, and honored scholars in return, and they bear the cost of a scholar class formed away from the legal questions their businesses, marriages, and illnesses actually raise. Most never encounter the trade-off stated in these terms.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, lay_communities, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__performance_only, lay_communities, beneficiary).

% Scholars in and adjacent to the tradition who publish the case that finite learning capacity belongs on the law that governs conduct now. Their essays circulate at the edge of the canonical conversation; no academy has adopted their reallocation proposals, and their careers sit outside the honor economy they critique.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakhic_pragmatists, excluded,
    moderate, biographical, mobile, national).

% University-based historians and philologists who trace how the sacrificial corpus moved from performed rite to classroom centerpiece after 70 CE, and who correlate curricular emphasis with institutional formation rather than with any performable demand. They describe the system from outside its authority claims and owe it no deference.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, academic_jewish_studies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__performance_only, rabbinic_scholarly_establishment).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the complete textual corpus intact across generations, transmits a shared analytic method through its most demanding material, and anchors communal boundaries in a common curriculum — solved once, centrally, instead of per-community.
% TRANSFER_FUNCTION: Moves prime scholarly years from students, and money from donor communities and state stipend systems, toward the institutions and authorities that administer total-corpus study; what returns to the payers takes the form of communal continuity, identity anchoring, and access to a scholar class.
% ABSENT_VOICES: Halakhic pragmatists who argue for reallocating scholarly capacity to live legal questions stand outside the canonical conversation; their proposals carry no curricular consequence. Students who privately question the allocation lack standing to raise it — questioning the curriculum reads as questioning the path. Donors never confront a stated counterfactual use for their earmarked funds.
% DISAPPEARANCE_RATIONALE: If commandment-status treatment of the sacrificial orders ended overnight, curricula would rebalance toward the law that governs conduct, ordination examinations would rewrite themselves, funding claims keyed to total-corpus mastery would lose their object, and the establishment's authority claim — expertise spanning the law as lived — would reconfigure around whatever replaced the sacrificial orders at the center. The texts survive in archives regardless; the institutional economy built on their commandment-status does not.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, the tradition's central commandments — the sacrificial code — lost their performance site. The arrangement was built to solve the problem of how the sacrificial commandments could retain authority, teachability, and communal centrality without an altar at which anything could be performed.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of rabbinic Judaism — outside the beneficiary set — attest the founding problem's original form and its dissolution: the post-70 programmatic substitution of study, prayer, and deeds of lovingkindness for the sacrificed cult, and the migration of the sacrificial corpus from performed law to curricular material whose modern emphasis tracks institutional formation (the academy system, stipended full-time study) rather than performative demand. No source outside the beneficiary set attests that the founding problem remains live in its original form; liveness claims originate with the establishment and its dependent constituencies.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the arrangement consumes prime scholarly years and earmarked funding for material that, on this reading's own terms, has no performable object; it is not maximal because the same structure genuinely preserves the corpus and transmits the analytic method — live functions ride in the same vehicle as the dead claim. Suppression (0.62) is a raw structural property, unscaled by power or scope: it combines institutional gates (sequence requirements, ordination prerequisites, stipend dependency) with internalized identity fusion (whole-corpus devotion as the community's supreme value), roughly half each per the omega below. Theater (0.46) reflects a growing share of activity that rehearses unperformable procedure — simulated-service manuals, reconstructed-institute liturgy, practical-law chapters on offerings no court can accept — alongside genuinely functional textual work. Accessibility_collapse (0.42) is moderate: alternatives exist inside the tradition (applied law, scripture, ethics) but carry status penalties and sit outside the honor economy. Resistance (0.28) is low: reallocation arguments surface at the margins and individual scholars quietly de-emphasize, but no organized challenge exists. The three measurement series share one grid (points 0-150 map to 1875-2025) so every metric is authored at every examined time point; trajectories rise monotonically — no oscillation is modeled — tracking the professionalization of the academy system, the postwar scale-up of stipended full-time study, and the hardening of enforcement machinery. The suppression_requirement series is included specifically because enforcement capacity changed over the interval: from diffuse communal expectation to formalized curricular gating and stipend-linked compliance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat experiences the arrangement as fidelity: mastering and teaching the whole Torah is the point of the enterprise, and demoting any order would betray the canon. The student seat experiences the same hours as diversion — life-years spent on procedure nothing can perform — with exit priced in identity rather than convenience. Lay funders straddle the divide: they purchase continuity and meaning while bearing the opportunity cost of a scholar class pointed away from the law their own lives raise. The engine computes these divergent per-seat classifications from the declared positions; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The establishment and the institutions sit near the beneficiary pole: the arrangement subsidizes their authority and revenue, and their exits are locked or costly. Students sit near the target pole, amplified by identity_locked exit — spent years cannot be repriced. Lay communities derive mid-range directionality from their dual declaration: they pay (funding, opportunity cost) and benefit (continuity, identity) through the same structure. Excluded pragmatists and the academic observer fall outside the derivation: one is structurally outside the conversation, the other analytical. No directionality overrides were needed — the beneficiary/victim declarations plus exit atoms already separate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — keep the sacrificial commandments authoritative and teachable pending their conditions — has, on this reading, outlived its object: the conditions are absent and the reading assigns the suspension no present demand. What persists is the institutional economy built around the mandate. Naming the type matters in both directions: calling the arrangement pure coordination erases the diversion of finite scholarly capacity; calling it pure extraction erases the genuine preservation and method-transmission work bundled in the same curriculum. The hybrid verdict keeps both facts on the table, and the R5 interview (status: dead, verdict: world_rearranges) records the mismatch that distinguishes inertial persistence from live function. Note also the identity-coordination gaming risk: 'this is who we are' framing is a classic cover for extraction, so the theater metric and the counterfactual-allocation omega are load-bearing checks on whether the identity function justifies the coupling or launders it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the performance_only reading of the kodashim_commandment_status kernel; what would adopting a sibling reading change structurally?',
    'No empirical resolution — the readings are rival framings of one kernel. Adoption of study_as_performance converts the same study hours into fulfillment (measured waste collapses toward coordination cost); adoption of messianic_deferral converts diversion into restoration-preparation carrying partial option value.',
    'Classification is reading-indexed: under study_as_performance the arrangement computes as low-extraction coordination; under messianic_deferral as moderate; under this reading as substantially extractive. Cross-reading comparison is valid only as contrast, never aggregation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: one kernel, three readings, three constraints with distinct epsilon over the same standing arrangement.').

omega_variable(
    temple_restoration_contingency,
    'Does credible possibility of future restoration give the current study investment option value, reducing effective waste?',
    'Not empirically resolvable — it turns on eschatological conviction. A behavioral proxy exists: whether institutions hedge by teaching portable applied skills alongside the traditional sequence reveals a revealed probability of restoration credit.',
    'High credited option value lowers effective extraction toward preparation-cost; zero credited option value pushes the arrangement toward pure diversion of finite scholarly capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_restoration_contingency, conceptual, 'Option value of sacrificial-law study under restoration uncertainty.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (curricular gates, stipend dependency, communal sanction) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit suppression trajectory: follow students who leave the track — if the pressure lifts once the barriers are gone, the suppression was structural; if guilt and damaged self-concept persist after exit, it was substantially internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure — targets carry the lock with them after exit, and curricular reform alone would not release the diverted capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized split of the suppression holding students to the full-corpus track.').

omega_variable(
    counterfactual_allocation_value,
    'What would the diverted scholarly capacity produce if redirected, and how much of sacrificial-order study is irreplaceable analytic training?',
    'Outcome comparison across curricula that weight the sacrificial orders differently: decisor competence, textual facility, long-run retention among graduates.',
    'High transfer value reclassifies part of the measured diversion as legitimate training cost (effective extraction falls); low transfer value confirms the diversion as deadweight (effective extraction rises).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_allocation_value, empirical, 'Whether sacrificial-order study is uniquely formative or substitutable by other demanding material.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t25, kodashim_commandment_status__performance_only, theater_ratio, 25, 0.21).
narrative_ontology:measurement_basis(koda_tr_t25, observed).
narrative_ontology:measurement(koda_tr_t50, kodashim_commandment_status__performance_only, theater_ratio, 50, 0.26).
narrative_ontology:measurement_basis(koda_tr_t50, observed).
narrative_ontology:measurement(koda_tr_t75, kodashim_commandment_status__performance_only, theater_ratio, 75, 0.31).
narrative_ontology:measurement_basis(koda_tr_t75, observed).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__performance_only, theater_ratio, 100, 0.37).
narrative_ontology:measurement_basis(koda_tr_t100, observed).
narrative_ontology:measurement(koda_tr_t125, kodashim_commandment_status__performance_only, theater_ratio, 125, 0.42).
narrative_ontology:measurement_basis(koda_tr_t125, observed).
narrative_ontology:measurement(koda_tr_t150, kodashim_commandment_status__performance_only, theater_ratio, 150, 0.46).
narrative_ontology:measurement_basis(koda_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t25, kodashim_commandment_status__performance_only, base_extractiveness, 25, 0.41).
narrative_ontology:measurement_basis(koda_be_t25, observed).
narrative_ontology:measurement(koda_be_t50, kodashim_commandment_status__performance_only, base_extractiveness, 50, 0.47).
narrative_ontology:measurement_basis(koda_be_t50, observed).
narrative_ontology:measurement(koda_be_t75, kodashim_commandment_status__performance_only, base_extractiveness, 75, 0.54).
narrative_ontology:measurement_basis(koda_be_t75, observed).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__performance_only, base_extractiveness, 100, 0.61).
narrative_ontology:measurement_basis(koda_be_t100, observed).
narrative_ontology:measurement(koda_be_t125, kodashim_commandment_status__performance_only, base_extractiveness, 125, 0.67).
narrative_ontology:measurement_basis(koda_be_t125, observed).
narrative_ontology:measurement(koda_be_t150, kodashim_commandment_status__performance_only, base_extractiveness, 150, 0.72).
narrative_ontology:measurement_basis(koda_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__performance_only, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t25, kodashim_commandment_status__performance_only, suppression_requirement, 25, 0.33).
narrative_ontology:measurement_basis(koda_su_t25, observed).
narrative_ontology:measurement(koda_su_t50, kodashim_commandment_status__performance_only, suppression_requirement, 50, 0.38).
narrative_ontology:measurement_basis(koda_su_t50, observed).
narrative_ontology:measurement(koda_su_t75, kodashim_commandment_status__performance_only, suppression_requirement, 75, 0.44).
narrative_ontology:measurement_basis(koda_su_t75, observed).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__performance_only, suppression_requirement, 100, 0.51).
narrative_ontology:measurement_basis(koda_su_t100, observed).
narrative_ontology:measurement(koda_su_t125, kodashim_commandment_status__performance_only, suppression_requirement, 125, 0.57).
narrative_ontology:measurement_basis(koda_su_t125, observed).
narrative_ontology:measurement(koda_su_t150, kodashim_commandment_status__performance_only, suppression_requirement, 150, 0.62).
narrative_ontology:measurement_basis(koda_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% The colloquial label 'status of the sacrificial commandments' conflates three structurally distinct claims (epsilon-invariance decomposition): messianic_deferral (suspension with a readiness-demand), performance_only (this file: suspension as husk, no present demand), and study_as_performance (study as fulfillment). Each instantiates a different constraint with its own epsilon over the same standing arrangement. This reading authors the highest epsilon because it credits the arrangement with no commandment-level function at all; study_as_performance authors the lowest because it credits every study hour as performance; messianic_deferral sits between via partial option value. Family members link through network.affects_constraints; upstream, the shared fixed text and its interpretive settlement feed all three readings' institutional consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
