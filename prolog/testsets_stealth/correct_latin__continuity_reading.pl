% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__continuity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Continuity Reading of Correct Latin: Transmitted Living Practice as the Standard
 *   domain: intellectual_history/historical_linguistics
 *
 * SUMMARY:
 *   From the Carolingian correction campaigns to the eve of print,
 *   correctness in Latin was governed by a single arrangement: the language
 *   is what its continuous chain of schools, scriptoria, and chanceries
 *   transmits, and medieval forms are that same language legitimately grown.
 *   This story instantiates the continuity reading of the correct_latin
 *   kernel as a clean, epsilon-invariant constraint over the interval
 *   800-1450. The arrangement solves a real civilizational problem — keeping
 *   one learned language alive without a state, print, or mass literacy —
 *   while the institutions that run the transmission chain collect
 *   gatekeeping authority and impose correction costs on the periphery. The
 *   claim and the metrics are authored independently: the claimed type is
 *   tangled_rope because the structure carries both a genuine coordination
 *   function and identifiable asymmetric payment; the metrics describe
 *   coordination-dominant operation with moderate extraction that peaks with
 *   scholastic institutionalization and declines as print and philological
 *   criticism diversify the criteria of correctness.
 *
 * KEY AGENTS:
 *   - cathedral_school_masters: primary agenda-setter (organized/identity_locked) — administers the grammar curriculum and the examination gates into every literate profession
 *   - curial_chancery: co-agenda-setter (institutional/identity_locked) — fixes documentary style for all western diplomacy and trains the chancery cadre
 *   - monastic_scriptoria: custodial beneficiary (organized/constrained) — reproduces the transmitted corpus as vowed labor and draws vocation from continuity
 *   - provincial_chancery_clerks: primary payer (moderate/constrained) — bears correction, retraining, and formulary costs far from the centers
 *   - regional_monastic_communities: peripheral payer-beneficiary (organized/constrained) — local usage repeatedly overwritten by visiting reformers
 *   - women_barred_from_grammar_schooling: excluded voice (powerless/constrained) — shaped by the arrangement with no seat in setting it
 *   - modern_historical_linguists: analytical observer (analytical/analytical) — reads the full transmission record from outside every period party
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.34).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.4).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Continuity Reading of Correct Latin: Transmitted Living Practice as the Standard").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "intellectual_history/historical_linguistics").

domain_priors:requires_active_enforcement(correct_latin__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, '8db756e3-eaf2-44aa-ab1d-1c80ba478a27').
narrative_ontology:cs_kernel_codification('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', implicit).
narrative_ontology:cs_authority_grounding('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', practice).
narrative_ontology:cs_interpretation_layer_present('8db756e3-eaf2-44aa-ab1d-1c80ba478a27').
narrative_ontology:cs_reading_relation('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', correct_latin__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', correct_latin__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', foundational, transmitted_usage_constitutes_correctness).
narrative_ontology:cs_axiom_status(transmitted_usage_constitutes_correctness, holdable).
narrative_ontology:cs_axiom_grounding('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', transmitted_usage_constitutes_correctness, conventional).
narrative_ontology:cs_axiom('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', foundational, living_practice_outranks_ancient_manuscripts).
narrative_ontology:cs_axiom_status(living_practice_outranks_ancient_manuscripts, holdable).
narrative_ontology:cs_axiom_grounding('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', living_practice_outranks_ancient_manuscripts, conventional).
narrative_ontology:cs_axiom('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', secondary, medieval_change_is_internal_adjustment).
narrative_ontology:cs_axiom_status(medieval_change_is_internal_adjustment, holdable).
narrative_ontology:cs_axiom_grounding('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', medieval_change_is_internal_adjustment, conventional).
narrative_ontology:cs_reference_frame('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', continuous_living_transmission).
narrative_ontology:cs_drift_state('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', incunable_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('8db756e3-eaf2-44aa-ab1d-1c80ba478a27', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, cathedral_school_masters).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, monastic_scriptoria).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, curial_chancery).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, provincial_chancery_clerks).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, regional_monastic_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, regional_monastic_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach grammar from Donatus and Priscian in the cathedral and monastery schools, examine candidates for orders and notarial office, and decide which new forms count as acceptable usage. Their income, rank, and the authority of their office rest on being the recognized transmitters of the language; stepping outside that role would mean surrendering the entire basis of their standing, so they defend the transmitted forms as a matter of who they are, not merely what they are paid.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, cathedral_school_masters, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, cathedral_school_masters, beneficiary).

% Drafts papal and imperial correspondence in house styles codified in the ars dictaminis manuals, returns or refuses letters that depart from those styles, and trains the clerks who staff chanceries across the West. Its diplomatic authority depends on its Latin being accepted everywhere as the same language; adopting any rival criterion of correctness would dissolve the universality on which its power rests.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, curial_chancery, agenda_setter,
    institutional, generational, identity_locked, continental).

% Copy scripture, the Fathers, and the liturgy in the received forms as daily labor undertaken under vow; the unbroken continuity of their hand with the ancient church is part of what sanctifies the work. They gain vocation, purpose, and institutional legitimacy from the transmitted stream, and they supply the physical reproduction that keeps the stream alive.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, monastic_scriptoria, beneficiary,
    organized, generational, constrained, continental).

% Draft contracts, wills, and court records far from the curial centers, then submit to correction when visiting legates or formally trained notaries mark their spellings and formulas as rustic. Conforming means retraining and purchasing the newer formularies; refusing means their documents lose legal weight. Their livelihood depends on staying inside the standard set elsewhere.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, provincial_chancery_clerks, payer,
    moderate, biographical, constrained, regional).

% Small houses at the edge of the network whose local copying traditions accumulate idiosyncrasies over generations; visiting reformers and bishops periodically order their books and charters rewritten to the mainstream forms. They depend on the shared language for their place in the order while paying, repeatedly, to have their own accumulated usage overwritten.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, regional_monastic_communities, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, regional_monastic_communities, beneficiary).

% Excluded almost entirely from the grammar schools through which the language is transmitted, they acquire literacy only through convent exceptions or household tutoring, and they hold no seat anywhere the standard is set, taught, or revised. Vernacular writing becomes the main outlet for literate ambition the Latin channel does not serve; the arrangement shapes their lives without their participation.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, women_barred_from_grammar_schooling, excluded,
    powerless, biographical, constrained, continental).

% Analyze the manuscript record, school statutes, and formularies from outside every period party, tracing which forms the transmission chain actually carried, where correction overwrote variation, and how quickly innovations propagated between regions. They hold no stake in the period's disputes over correctness and can see the whole structure at once.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, modern_historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(correct_latin__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single supraregional learned language across a politically fragmented, low-literacy Europe with no printing and no mass education: teacher-to-student transmission gives every learner the same usable standard, lets documents and letters circulate and be read from Ireland to Poland, and keeps the inherited corpus of law, scripture, and learning accessible without manuscript archaeology.
% TRANSFER_FUNCTION: Moves conformity labor and correction costs from peripheral writers and learners toward the forms preferred by the custodial center; moves status, fee income, and gatekeeping authority to the institutions that control schooling and chancery style; and moves the written record itself into standardized transmitted forms, sometimes overwriting local variants in the process.
% ABSENT_VOICES: Women barred from the grammar schools had no seat where the standard was set; vernacular authors, whose languages developed in the shadow of the Latin monopoly on literacy and administration, were outside the conversation; regional writers whose idioms were regularized away objected locally but left little record; and the Greek East, which never accepted Western claims about where antiquity lived, simply declined to participate.
% DISAPPEARANCE_RATIONALE: If the transmission standard vanished overnight, the school chain would lose its criterion and written Latin would fragment into mutually opaque regional varieties, as Romance did wherever the chain broke; the administrative unity of western Christendom would collapse with it, cross-border learned communication would fail, and access to the legal and theological corpus would narrow to those who could reconstruct it from scattered manuscripts.
% FOUNDING_PROBLEM: After the collapse of Roman administrative infrastructure, spoken Latin was diverging into Romance vernaculars while law, liturgy, diplomacy, and learning still required one common language; the founding problem was keeping a single learned language alive and teachable once the state that had sustained it was gone.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: secular royal and city chanceries that resented clerical control nonetheless paid for Latin training because they needed the language; Mediterranean merchants and the Toledo translation movement required Latin as the working target language; the Romance vernaculars themselves attest the arrangement's function, since Latin died as a living medium exactly where the school chain broke; and modern paleography confirms that the Carolingian correction campaigns measurably re-unified written usage across regions.
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__continuity_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__continuity_reading_tests).
:- end_tests(correct_latin__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.34 at interval end) because the conformity costs the standard imposes are real but thin: correction of spellings and formulas, purchase of formularies, retraining after reform visits — not surrender of livelihoods, which remain possible inside the system. Suppression (0.40) reflects structural enforcement — document validity, ordination examinations, notarial licensing all route through conformance — with weak internalized component, since dissidents exited into vernaculars rather than defending the Latin standard while secretly resenting it. Theater ratio (0.35) tracks the growth of grammatical lore beyond practical need: etymological mythologies and speculative grammar that legitimated the masters without improving writers, rising sharply once defense of the standard became ritualized against critics. Accessibility collapse (0.48) is mid-range: within the continuity frame, textual reconstruction barely presents itself as an option — it is absorbed as an error category — but vernacular exit remained genuinely open throughout, so alternatives never fully collapsed. Resistance (0.40) registers periodic regional pushback, complaints about schoolmaster pedantry, and the slow build of external criticism late in the interval. All three series run on one shared seven-point grid (800, 950, 1100, 1250, 1350, 1400, 1450) so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the custodial seats compute differently from identical structural data. From the cathedral school and the curia, the arrangement is stewardship: they experience correction as care for a shared inheritance and cannot exit without dissolving their own authority. From a provincial clerk's desk, the same correction arrives as distant strangers invalidating his work product and taxing his career. Two clerks of nominally equal standing sit in opposite positions: a curial-trained notary inside the style is a beneficiary of the gate his training bought, while a provincial clerk outside it pays the toll — same profession, same century, opposite directionality, differentiated entirely by constraint-specific position in the transmission chain rather than by global power.
 *
 * DIRECTIONALITY LOGIC:
 *   Cathedral school masters and the curial chancery derive strong beneficiary directionality from their declarations plus identity-locked exit: the standard subsidizes their authority, and they cannot leave it without ceasing to be what they are. Monastic scriptoria derive beneficiary directionality with dampened amplitude — their constrained exit and their payment of copying labor pull them back toward symmetry. Provincial chancery clerks and regional monastic communities derive target directionality amplified by constrained exit: they bear the transfer and cannot arbitrage it. Women excluded from grammar schooling are the one seat the automatic derivation handles poorly: excluded agents risk falling back to a symmetric default, but their structural position — denied access to the benefit stream the arrangement monopolizes — places them nearer the target end, hence the explicit override at d=0.62 for the powerless atom. Suppression is authored as a raw structural property and is deliberately not scaled; only extractiveness rides directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping one learned language alive after the Roman state — is live across the entire interval, and the arrangement still performs its function at 1450, so no mandate has outlived its function and no zombie flag is warranted: founding_problem_status=live combined with disappearance_verdict=world_rearranges is the coherent pairing. The classification work the type distinction does here is preventive: reading the arrangement as pure coordination would erase the custodial rents and the peripheral correction costs that the same structure carries; reading it as pure extraction would erase the fact that the transmission chain was the only mechanism available that actually kept a continental learned language alive for six centuries, and that its beneficiaries were also its unpaid workforce. The tangled_rope claim holds both truths in one structure. Had the interval extended past print and the humanist victory, the analysis would change: a standard defended ritually after its function migrated elsewhere would present a very different profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading (continuity_reading) of the correct_latin kernel; how would instantiating a sibling reading change the structural data?',
    'Author the sibling stories (correct_latin__discontinuity_reading, correct_latin__hybrid_reading) over the same referent and compare computed classifications; the family decomposition is documented in each file''s network note.',
    'Under the discontinuity reading the same historical arrangement acquires a deception charge — schools transmitting corrupted forms as correct — raising epsilon substantially and recasting the masters as beneficiaries of a cover story; under the hybrid reading the victim set narrows to cases where textual evidence contradicts the stream. Classification of this file is valid only for the continuity reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story is one reading of a contested kernel, not the topic whole.').

omega_variable(
    correctness_criterion_location,
    'Where exactly does the kernel contest bite — is the dispute over the constitutive criterion of correctness (practice vs. text vs. both), or merely over application of a shared criterion?',
    'Conceptual analysis of the readings'' own texts: medieval grammatical prologues, humanist polemics, and modern editorial practice each state the criterion explicitly; if all three name different criteria, the dispute is constitutive and the readings are genuinely distinct constraints.',
    'If constitutive, the three readings cannot be merged or averaged and the foreclosure relations hold; if merely applicative, the family collapses toward one constraint with parameter disagreement and the reading_relations should be downgraded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(correctness_criterion_location, conceptual, 'Locates the structural element on which the sibling readings actually disagree.').

omega_variable(
    carolingian_correctio_net_effect,
    'Did the Carolingian correction campaigns produce net unification or net destruction — how much genuine variant material was permanently overwritten in the name of restoring the transmitted standard?',
    'Manuscript stemmatics and palimpsest recovery comparing pre-correction copies with their corrected descendants; surviving insular and Visigothic witnesses against Carolingian reform lines.',
    'If destruction dominates, early-interval extractiveness is understated and the 800-950 series should trend higher; if unification dominates, the coordination reading of the founding decades strengthens and the low early epsilon is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carolingian_correctio_net_effect, empirical, 'Whether the interval''s opening enforcement episode was coordination cost or irreversible loss.').

omega_variable(
    custodial_identity_lock_depth,
    'How deep is the custodial seats'' identity lock — is the masters'' and curia''s inability to abandon the transmitted standard constituted identity fusion, or ordinary career and institutional interest that would yield quickly to a better-paying alternative?',
    'Post-1450 adoption trajectories: institutions that converted rapidly to humanist curricula and textual standards once rewards shifted reveal shallow lock; institutions that defended transmitted forms against their own interest for generations reveal genuine fusion.',
    'Shallow lock lowers effective directionality toward the beneficiary end for the custodial seats and softens the asymmetry reading; deep lock sustains it and predicts prolonged resistance to any criterion shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_identity_lock_depth, empirical, 'Depth of the identity fusion binding the agenda-setting seats to the transmitted standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 800, 1450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clcont_tr_t800, correct_latin__continuity_reading, theater_ratio, 800, 0.16).
narrative_ontology:measurement(clcont_tr_t950, correct_latin__continuity_reading, theater_ratio, 950, 0.19).
narrative_ontology:measurement(clcont_tr_t1100, correct_latin__continuity_reading, theater_ratio, 1100, 0.24).
narrative_ontology:measurement(clcont_tr_t1250, correct_latin__continuity_reading, theater_ratio, 1250, 0.29).
narrative_ontology:measurement(clcont_tr_t1350, correct_latin__continuity_reading, theater_ratio, 1350, 0.27).
narrative_ontology:measurement(clcont_tr_t1400, correct_latin__continuity_reading, theater_ratio, 1400, 0.31).
narrative_ontology:measurement(clcont_tr_t1450, correct_latin__continuity_reading, theater_ratio, 1450, 0.35).

% Extraction over time
narrative_ontology:measurement(clcont_be_t800, correct_latin__continuity_reading, base_extractiveness, 800, 0.3).
narrative_ontology:measurement(clcont_be_t950, correct_latin__continuity_reading, base_extractiveness, 950, 0.31).
narrative_ontology:measurement(clcont_be_t1100, correct_latin__continuity_reading, base_extractiveness, 1100, 0.37).
narrative_ontology:measurement(clcont_be_t1250, correct_latin__continuity_reading, base_extractiveness, 1250, 0.42).
narrative_ontology:measurement(clcont_be_t1350, correct_latin__continuity_reading, base_extractiveness, 1350, 0.41).
narrative_ontology:measurement(clcont_be_t1400, correct_latin__continuity_reading, base_extractiveness, 1400, 0.37).
narrative_ontology:measurement(clcont_be_t1450, correct_latin__continuity_reading, base_extractiveness, 1450, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(clcont_su_t800, correct_latin__continuity_reading, suppression_requirement, 800, 0.36).
narrative_ontology:measurement(clcont_su_t950, correct_latin__continuity_reading, suppression_requirement, 950, 0.27).
narrative_ontology:measurement(clcont_su_t1100, correct_latin__continuity_reading, suppression_requirement, 1100, 0.38).
narrative_ontology:measurement(clcont_su_t1250, correct_latin__continuity_reading, suppression_requirement, 1250, 0.46).
narrative_ontology:measurement(clcont_su_t1350, correct_latin__continuity_reading, suppression_requirement, 1350, 0.47).
narrative_ontology:measurement(clcont_su_t1400, correct_latin__continuity_reading, suppression_requirement, 1400, 0.43).
narrative_ontology:measurement(clcont_su_t1450, correct_latin__continuity_reading, suppression_requirement, 1450, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__continuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'correct Latin' decomposes into three structurally distinct normative regimes — continuity (this file), discontinuity, and hybrid — each with its own epsilon, beneficiary/victim structure, and classification, per the epsilon-invariance principle. The continuity reading is the historically upstream member: it is the standing arrangement the other two readings define themselves against, and its institutional machinery (schools, chanceries, scriptoria) is the object the discontinuity reading proposes to replace and the hybrid reading proposes to correct. Each family member links the others via affects_constraints; no single story may hedge epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__continuity_reading, powerless, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
