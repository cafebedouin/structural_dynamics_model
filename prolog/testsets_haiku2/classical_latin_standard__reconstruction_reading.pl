% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__reconstruction_reading, []).

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
 *   constraint_id: classical_latin_standard__reconstruction_reading
 *   human_readable: Classical Latin Reconstruction Standard (Humanist Reading)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   The reconstruction reading of the Classical Latin standard emerges in
 *   Renaissance humanist circles (roughly 14th–16th centuries) as a response
 *   to perceived medieval corruption of the Latin language. Humanist
 *   philologists, trained in close textual analysis and Classical grammar,
 *   posit that correct Latin is the form recoverable only through
 *   archaeological return to the earliest and best manuscripts, requiring
 *   discontinuous break with medieval institutional practice. Medieval Latin
 *   users — cathedral schools, monasteries, universities, ecclesiastical
 *   institutions — find their linguistic practice retroactively delegitimized
 *   as corruption. The constraint operates as coordination (unified standard,
 *   textual fidelity) layered with extraction (systematic gatekeeping,
 *   prestige redistribution, retraining burdens on non-humanist
 *   practitioners). This is ONE reading of the classical_latin_standard
 *   kernel; sibling readings (continuity_reading, hybrid_reading) instantiate
 *   different constraints with different beneficiary structures and different
 *   ε values.
 *
 * KEY AGENTS:
 *   - humanist_philologists: Institutional agenda-setters with control of method and standard-setting authority; benefit from prestige and epistemic monopoly.
 *   - medieval_institutional_latin_users: Organized but structurally outmaneuvered payers; face delegitimization and costly retraining or subordination.
 *   - ecclesiastical_practitioners: Powerful but identity-locked; Latin continuity is bound to their institutional function but also their authority.
 *   - non_humanist_clergy: Moderate power, identity-locked; bear direct retraining costs with minimal resources or exit options.
 *   - italian_merchant_class: Powerful beneficiaries (without direct cost) who sponsor humanist academies and profit from prestige.
 *   - university_magisters: Excluded agenda-setters whose traditional authority is now delegitimized.
 *   - printing_press_operators: Beneficiaries who profit from market demand for newly edited Classical texts.
 *   - royal_courts: Observer institutional actors who adopt the standard as a marker of civilizational legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.78).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.81).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Classical Latin Reconstruction Standard (Humanist Reading)").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, 'c92961d9-cf31-4da0-a7c7-e849035c28df').
narrative_ontology:cs_kernel_codification('c92961d9-cf31-4da0-a7c7-e849035c28df', distributed).
narrative_ontology:cs_authority_grounding('c92961d9-cf31-4da0-a7c7-e849035c28df', extraction).
narrative_ontology:cs_interpretation_layer_present('c92961d9-cf31-4da0-a7c7-e849035c28df').
narrative_ontology:cs_reading_relation('c92961d9-cf31-4da0-a7c7-e849035c28df', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('c92961d9-cf31-4da0-a7c7-e849035c28df', classical_latin_standard__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('c92961d9-cf31-4da0-a7c7-e849035c28df', foundational, medieval_drift_is_corruption).
narrative_ontology:cs_axiom_status(medieval_drift_is_corruption, holdable).
narrative_ontology:cs_axiom_grounding('c92961d9-cf31-4da0-a7c7-e849035c28df', medieval_drift_is_corruption, empirically_contingent).
narrative_ontology:cs_axiom('c92961d9-cf31-4da0-a7c7-e849035c28df', foundational, philological_archaeology_recovers_authentic_form).
narrative_ontology:cs_axiom_status(philological_archaeology_recovers_authentic_form, holdable).
narrative_ontology:cs_axiom_grounding('c92961d9-cf31-4da0-a7c7-e849035c28df', philological_archaeology_recovers_authentic_form, deontological).
narrative_ontology:cs_reference_frame('c92961d9-cf31-4da0-a7c7-e849035c28df', philological_epistemic_authority).
narrative_ontology:cs_drift_state('c92961d9-cf31-4da0-a7c7-e849035c28df', institutional_consolidation_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('c92961d9-cf31-4da0-a7c7-e849035c28df', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, latin_elite_educators).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, medieval_institutional_latin_users).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, ecclesiastical_practitioners).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, non_humanist_clergy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, ecclesiastical_practitioners).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, italian_merchant_class).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, printing_press_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classical scholars with training in textual reconstruction, manuscript analysis, and Classical grammar. They establish and enforce standards of correctness through humanist academies, printed editions, and educational authority. Control the apparatus for determining which forms are 'correct' through philological method. Benefit from exclusive epistemic authority and prestige associated with recovering the 'authentic' Classical form.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).

% Cathedral schools, monastic communities, university magisters, and clerks whose Latin was formed through continuous medieval practice, abbreviations, technical vocabulary, and institutional conventions. Their Latin is now retroactively delegitimized as 'corrupted' or 'barbarous.' They must either abandon decades of linguistic practice, undergo expensive retraining, or accept subordinate status as practitioners of 'incorrect' Latin. Exit is costly: their authority derives from institutional position, not from personal retraining flexibility.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, medieval_institutional_latin_users, payer,
    organized, biographical, constrained, continental).

% The Roman Church uses Latin as its liturgical and administrative language. Medieval Latin evolved ecclesiastical vocabulary and forms essential to its institutional function (liturgical terms, theological neologisms, legal formulae). The reconstruction reading demands these be treated as corruptions and replaced with Classical equivalents that lack the same functional specificity. Church authority is partly bound to linguistic continuity with medieval tradition, but faces pressure to adopt 'correct' Classical form as a marker of learned legitimacy.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, ecclesiastical_practitioners, payer,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__reconstruction_reading, ecclesiastical_practitioners, beneficiary).

% Priests and monks in dioceses and monasteries without access to humanist education. Their Latin, shaped by local teaching and practice, is now deemed inadequate. Retraining requires travel to humanist centers, costs, and adoption of methods that delegitimize their own institutional lineage. Identity is deeply fused with ecclesiastical practice; exit from the Church is unthinkable, but exit from traditional Latin is treated as necessary for legitimacy within the Church.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, non_humanist_clergy, payer,
    moderate, biographical, identity_locked, regional).

% Wealthy merchant families in Florence, Venice, and other commercial centers who sponsor humanist scholars and academies, commission manuscript collections, and promote Classical learning as a marker of cultural refinement and civilizational superiority. They benefit from the prestige and networking value of humanist patronage without bearing the retraining cost (they employ secretaries and scholars who do).
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, italian_merchant_class, beneficiary,
    powerful, biographical, mobile, regional).

% Medieval university masters (at Bologna, Paris, Oxford) who taught Latin grammar and rhetoric through traditional methods based on medieval standards. They would argue that institutional continuity and functional stability of medieval Latin outweigh philological purity. They are structurally excluded from setting standards once the reconstruction reading gains dominance — their authority derives from the very tradition that is now delegitimized.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, university_magisters, excluded,
    organized, biographical, constrained, continental).

% Early printers who benefit from the humanist demand for correct Classical texts: they commission editions, employ humanist scholars as editors, and profit from the market for beautifully printed Classical works. The reconstruction standard creates demand for newly edited and emended Classical texts, driving a profitable market segment.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, printing_press_operators, beneficiary,
    organized, biographical, mobile, continental).

% European courts and princely patrons who see adoption of humanist Classical Latin as a marker of civilizational legitimacy and cultural authority. They observe the constraint from a position of power to shape institutional adoption through patronage and policy, but do not directly enforce the standard themselves.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, royal_courts, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(classical_latin_standard__reconstruction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, scientifically grounded standard for correct Latin based on recovered Classical texts and philological method, replacing fragmented medieval standards with unified norms. Enables cross-institutional communication through a shared understanding of grammatical correctness and authentic form.
% TRANSFER_FUNCTION: Moves epistemic authority from institutional practitioners (medieval cathedral schools, monasteries, universities) to a new class of philologically trained humanist scholars who control the methods and criteria for determining correctness. Also moves prestige and cultural capital toward those institutions (humanist academies, courts) that adopt the reconstruction standard.
% ABSENT_VOICES: Medieval linguistic practitioners and their intellectual descendants are largely excluded from the conversation about correctness — they are treated as having corrupted the language rather than developed it. Non-humanist clergy and university magisters would testify that their Latin served institutional and functional purposes effectively and that discontinuous reconstruction imposes costly retraining without demonstrable functional gain. Oral tradition practitioners and scribal communities whose Latin work through inscription and practice rather than textual analysis are structurally absent from the philological apparatus.
% DISAPPEARANCE_RATIONALE: If the reconstruction reading and its enforcement mechanisms disappeared, institutional Latin would continue to evolve through medieval and post-medieval practice. Ecclesiastical Latin would retain its functional vocabulary. University teaching would revert to traditional grammar methods. The prestige economy that ties cultural legitimacy to Classical purity would collapse, and medieval institutional forms would no longer carry the stigma of corruption. The market for newly edited Classical texts would shrink dramatically.
% FOUNDING_PROBLEM: Medieval Latin evolved away from Classical norms through centuries of practice, creating divergence in vocabulary, syntax, and forms. This drift made recovery of Classical texts and precision in Classical scholarship difficult — scholars reading Cicero through medieval lenses could misread meaning and miss nuance. The founding problem is: how can we read Classical authors accurately given that our inherited linguistic framework has shifted?
% FOUNDING_PROBLEM_CORROBORATION: Humanist philologists attest the problem is live and worsening: medieval glosses and commentaries on Classical texts often misread the originals due to linguistic drift. However, medieval institutional practitioners and ecclesiastical authorities attest the problem is overstated: medieval Latin works well for its institutional purposes; Classical precision is a scholarly luxury, not a functional necessity. Independent evidence from the effectiveness of medieval theological and legal writing suggests medieval practitioners solved their communicative problems adequately.
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__reconstruction_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__reconstruction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__reconstruction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the reconstruction reading creates a new class hierarchy: only those trained in philological method control correctness; medieval practitioners' decades of linguistic competence become worthless. Suppression is equally high (0.81) because enforcement requires active delegitimization of medieval forms and gatekeeping of humanist training. Theater is moderate (0.29): genuine philological work happens, but an increasing share of activity serves to defend the status hierarchy (new editions prove the Classical standard is recoverable; medieval marginal glosses are dismissed rather than engaged). The measurement series show extraction and suppression rising together as humanist institutions consolidate power over the century-and-a-quarter interval (t0~1350 projection, t25~1475 observed consolidation). Accessibility collapse (0.72) reflects that medieval practitioners cannot easily maintain linguistic authority once the epistemic framework shifts — reverting to medieval norms is now coded as ignorance. Resistance (0.58) is substantial: university magisters, monasteries, and churches actively resist the standard or adopt it partially, maintaining medieval forms in functional contexts.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist philologist seat, the reconstruction reading is coordination: recovering the authentic Classical text and establishing unified standards for correctness. From the medieval practitioner seat, it is extraction: systematic delegitimization of their authority and forced retraining under a new institutional apparatus they did not design. The engine computes per-seat classification: the humanist seat should classify as rope or genuine coordination; the medieval institutional seat should classify as snare or tangled_rope. The divergence measures the constraint's asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists are full beneficiaries (d near 0.0): they gain epistemic authority, prestige, institutional resources, and control of the standard-setting apparatus. Medieval institutional users are targets (d near 1.0): their authority is eroded, their competence is delegitimized, retraining is costly and often identity-threatening. Ecclesiastical practitioners sit higher on the target end (d~0.85) because their identity is fused with linguistic practice and their institutional function requires Latin continuity — the constraint forces a choice between institutional integrity and learned legitimacy. Non-humanist clergy are maximal targets (d~0.95): powerless, identity-locked, bearing direct retraining burden with no epistemic authority to contest the standard. Merchant patrons and printers are beneficiaries (d near 0.0) by proximity to humanists, though their benefit is indirect. University magisters are excluded rather than direct payers — their authority simply ceases to count, which is a form of extraction (loss of institutional legitimacy and resource allocation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is genuine but contested: medieval drift does make Classical texts harder to read with medieval lenses, but medieval Latin solves its institutional problems adequately. The constraint prevents the mismatch: founding_problem_status=contested, disappearance_verdict=world_rearranges (the world would reorganize around evolved medieval Latin, not classical reconstruction). This mismatch (contested status + world-rearranges verdict) signals capture/zombie: the founding problem has been reframed (from practical Classical reading to epistemic purity) but the constraint persists. Theater ratio rising from 0.08 to 0.29 suggests increasing proportion of activity serves status maintenance rather than the original problem (reading Classical texts accurately). Mandatrophy is not yet fully resolved but the delta is visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reconstruction_vs_continuity_epistemic_ground,
    'Is textual recovery through philological archaeology epistemically superior to knowledge preserved through unbroken institutional practice, or do the two methods capture different dimensions of linguistic truth?',
    'Long-term outcome comparison: does reconstructed Classical Latin enable communicative tasks (theology, law, science) as effectively as evolved medieval forms? Does adoption of the reconstruction standard lead to documented loss of meaning or functional degradation in domains that previously used medieval Latin?',
    'If practice-based knowledge proves superior or equivalent for institutional purposes, the reconstruction reading''s claim to exclusive correctness fails — the constraint becomes pure extraction masquerading as scholarship. If textual recovery demonstrates genuine communicative advantage, the reading''s foundation strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_vs_continuity_epistemic_ground, empirical, 'Whether philological textual recovery or institutional practice-preservation is epistemically privileged.').

omega_variable(
    medieval_corruption_or_development,
    'Is medieval Latin change properly characterized as corruption and drift, or as legitimate linguistic development responding to institutional needs and changed communicative contexts?',
    'Comparative analysis: do medieval innovations solve problems (ecclesiastical terminology, theological precision, legal specificity) that Classical forms do not address? Would reconstructed Classical Latin create gaps in functional vocabulary that medieval practitioners had filled?',
    'If medieval change is development (not corruption), the delegitimization of medieval forms is arbitrary gatekeeping; if corruption, the reconstruction reading''s normative claim holds. This is the foundational axiom boundary between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_corruption_or_development, conceptual, 'Whether medieval Latin drift is corruption or legitimate development.').

omega_variable(
    suppression_structural_or_internalized,
    'Is the measured suppression of medieval Latin forms structural (institutional barriers, gatekeeping, resource control) or internalized (medieval practitioners'' acceptance that their forms ARE corrupted)?',
    'Post-humanist outcome: where medieval practitioners or their institutional descendants maintain Latin literacy outside humanist frameworks, does suppression persist or does Latin practice revert to local development? Do communities that lose access to humanist education recreate Medieval-like forms?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — practitioners carry delegitimization with them. If structural, removing access to humanist gatekeeping would restore use of medieval forms. This informs whether the constraint is a true tangled rope (genuine coordination + asymmetric extraction) or closer to pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Whether suppression of medieval forms is structural gatekeeping or internalized acceptance of ''corruption''.').

omega_variable(
    kernel_committer_choice,
    'This constraint instantiates the reconstruction_reading of the classical_latin_standard kernel. What separates this reading from the continuity_reading and hybrid_reading variants?',
    'The readings are distinguished by their foundational axioms: this reading holds that medieval change IS corruption and that only philological archaeology recovers truth. The continuity reading holds that medieval change is legitimate development. The hybrid reading holds that both Classical norms AND legitimate medieval innovations have authority. Each reading produces a different constraint with different ε and beneficiary/victim structures.',
    'If the reconstruction reading is displaced by the continuity or hybrid readings gaining institutional dominance, the constraint''s classification shifts: extraction become coordination, suppression becomes cooperation. The commission exists to measure which reading the institutions actually settle on.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_choice, conceptual, 'Kernel reading identity and committer-axis positioning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__reconstruction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(clas_tr_t5, classical_latin_standard__reconstruction_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(clas_tr_t10, classical_latin_standard__reconstruction_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(clas_tr_t15, classical_latin_standard__reconstruction_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(clas_tr_t20, classical_latin_standard__reconstruction_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(clas_tr_t25, classical_latin_standard__reconstruction_reading, theater_ratio, 25, 0.29).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__reconstruction_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clas_be_t5, classical_latin_standard__reconstruction_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clas_be_t10, classical_latin_standard__reconstruction_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(clas_be_t15, classical_latin_standard__reconstruction_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(clas_be_t20, classical_latin_standard__reconstruction_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(clas_be_t25, classical_latin_standard__reconstruction_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__reconstruction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clas_su_t5, classical_latin_standard__reconstruction_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(clas_su_t10, classical_latin_standard__reconstruction_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(clas_su_t15, classical_latin_standard__reconstruction_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(clas_su_t20, classical_latin_standard__reconstruction_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(clas_su_t25, classical_latin_standard__reconstruction_reading, suppression_requirement, 25, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, information_standard).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__reconstruction_reading, 0.12).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel spawns three constraint stories, one per reading. Each reading instantiates a different constraint with different ε and different beneficiary/victim structures. The reconstruction_reading (this file) suppresses medieval forms as corruption and concentrates epistemic authority in the humanist class. The continuity_reading treats medieval evolution as legitimate and preserves institutional authority in practicing communities. The hybrid_reading creates a dual standard and distributes authority across both frameworks. The three constraints are related by the kernel they interpret, not by a causal chain — they are simultaneous alternative readings of the same commitment, not sequential evolution. Network edges track which reading influences which: the reconstruction reading, as it gains institutional dominance, creates pressure on continuity and hybrid readings to justify their alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__reconstruction_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
