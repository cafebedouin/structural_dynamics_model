% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Continuity Reading: Medieval Latin as Living Evolved Practice
 *   domain: intellectual/linguistic/institutional
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the contested kernel
 *   'correct_latin': the claim that Latin is the form transmitted through
 *   continuous living practice, and medieval Latin is legitimate evolved
 *   Classical Latin. Under this reading, medieval writers and practitioners
 *   (clergy, scribes, scholars) benefit from a linguistic framework that
 *   validates their evolved forms—phonetic shifts, case reductions,
 *   vocabulary borrowing—as natural linguistic development rather than
 *   corruption. The Church and ecclesiastical institutions function as the
 *   agenda-setters, transmitting Latin across the medieval centuries in forms
 *   that diverge from Classical texts but remain internally coherent and
 *   functionally adequate. The constraint operates by establishing a
 *   legitimacy claim grounded in practice-based authority: what practitioners
 *   actually do, across centuries, constitutes correctness. This reading
 *   coexists with a discontinuity reading (Classical forms as sole legitimate
 *   standard) and a hybrid reading (Classical forms with targeted textual
 *   correction). The three readings emit structurally distinct constraints
 *   with different beneficiary/victim structures, different extraction
 *   profiles, and different authority groundings.
 *
 * KEY AGENTS:
 *   - medieval_latin_practitioners: users of evolved Latin forms in ecclesiastical, legal, and scholarly contexts; benefit from validation of their practice
 *   - ecclesiastical_institutions: preserve and transmit Latin liturgy and administration; function as agenda-setters grounding legitimacy in living tradition
 *   - vernacular_language_communities: Romance speakers; benefit from framing language divergence as natural evolution rather than corruption
 *   - classical_philologists: analyze medieval texts; constrained by loss of exclusive authority to declare forms 'incorrect'
 *   - renaissance_humanists: excluded from this reading; would champion Classical purism and textual reconstruction
 *   - textual_tradition_keepers: monasteries, cathedral schools; validate their practice of copying medieval Latin without correction
 *   - analytical_observer: examines the structure of the contest itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.31).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.22).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, rope).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Continuity Reading: Medieval Latin as Living Evolved Practice").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "intellectual/linguistic/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, '812a92bb-24f1-4b1a-8373-5d62bef1f07e').
narrative_ontology:cs_kernel_codification('812a92bb-24f1-4b1a-8373-5d62bef1f07e', fixed_text).
narrative_ontology:cs_authority_grounding('812a92bb-24f1-4b1a-8373-5d62bef1f07e', practice).
narrative_ontology:cs_interpretation_layer_present('812a92bb-24f1-4b1a-8373-5d62bef1f07e').
narrative_ontology:cs_reading_relation('812a92bb-24f1-4b1a-8373-5d62bef1f07e', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('812a92bb-24f1-4b1a-8373-5d62bef1f07e', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('812a92bb-24f1-4b1a-8373-5d62bef1f07e', foundational, living_practice_grounds_legitimacy).
narrative_ontology:cs_axiom_status(living_practice_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('812a92bb-24f1-4b1a-8373-5d62bef1f07e', living_practice_grounds_legitimacy, conventional).
narrative_ontology:cs_axiom('812a92bb-24f1-4b1a-8373-5d62bef1f07e', secondary, linguistic_evolution_is_natural).
narrative_ontology:cs_axiom_status(linguistic_evolution_is_natural, holdable).
narrative_ontology:cs_axiom_grounding('812a92bb-24f1-4b1a-8373-5d62bef1f07e', linguistic_evolution_is_natural, empirically_contingent).
narrative_ontology:cs_reference_frame('812a92bb-24f1-4b1a-8373-5d62bef1f07e', living_latin_transmission).
narrative_ontology:cs_drift_state('812a92bb-24f1-4b1a-8373-5d62bef1f07e', high_medieval_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('812a92bb-24f1-4b1a-8373-5d62bef1f07e', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, medieval_latin_practitioners).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, vernacular_language_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, textual_tradition_keepers).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, classical_philologists).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, linguistic_evolution_naturalness).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, living_language_legitimacy).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, practice_based_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clergy, scribes, and scholars using Latin in ongoing institutional contexts (liturgy, legal documents, scholarly correspondence). Under the continuity reading, their evolved forms—phonological shifts, case-system simplification, vocabulary from vernacular substrates—are legitimate developments of Latin, not deviations. They benefit from this framing by validating their actual practice without requiring constant appeal to ancient texts for legitimacy.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, medieval_latin_practitioners, beneficiary,
    moderate, generational, constrained, continental).

% The Church preserves Latin liturgy and administrative language across the medieval period. The continuity reading justifies maintaining evolving medieval Latin forms in Church practice without requiring reconstruction to Classical norms. The Church controls the transmission chain and benefits from a legitimacy framework that treats living practice as authoritative, reducing the need to constantly revise usage against distant classical texts.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, ecclesiastical_institutions, agenda_setter,
    institutional, civilizational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, ecclesiastical_institutions, beneficiary).

% As Romance languages diverge from Latin during the medieval period, the continuity reading treats this divergence as legitimate evolution rather than corruption. Speakers of early French, Italian, Iberian languages benefit from a framework that validates their linguistic innovations as natural developments of Latin substrate, not as debasement.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, vernacular_language_communities, beneficiary,
    organized, generational, mobile, continental).

% Scholars trained in Classical Latin forms encounter medieval texts written in evolved forms that diverge from Classical norms (ablative forms replaced by prepositional constructions, phonetic spelling reflecting sound changes, new vocabulary). Under the continuity reading, these divergences are valid linguistic evolution, not errors requiring correction. This reading constrains the philologist's authority to declare medieval forms 'incorrect'—they must analyze rather than correct.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, classical_philologists, payer,
    moderate, biographical, constrained, regional).

% Later Renaissance scholars who championed Classical purism and textual reconstruction would find the continuity reading's acceptance of medieval evolution hostile to their philological project. They are excluded from the medieval period's self-understanding under this reading—their corrective impulse presupposes the discontinuity reading, which they would advance but cannot dominate during the medieval centuries themselves.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, renaissance_humanists, excluded,
    organized, biographical, trapped, regional).

% Monasteries, cathedral schools, and scribal centers that preserve, copy, and transmit texts. The continuity reading validates their practice of copying medieval Latin works without treating the medieval forms as departures from an ideal Classical text. Their reproduction and transmission of living medieval Latin is legitimate cultural work, not preservation of corrupted versions awaiting later correction.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, textual_tradition_keepers, beneficiary,
    powerful, civilizational, mobile, continental).

% Historical linguists, literary scholars, and philosophers of language who examine the structure of the contest itself: what counts as 'correct,' who adjudicates legitimacy, and what authority grounds normative claims about language evolution.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Validates the ongoing use of Latin as a living written language across medieval Europe by treating evolved medieval forms as legitimate linguistic development rather than requiring reconstruction to Classical norms. This allows a unified linguistic community spanning centuries to operate without the constant friction of correction-against-ancient-texts. Practitioners can write, read, teach, and transmit Latin confidently in the forms available to them.
% TRANSFER_FUNCTION: Moves authority over 'correctness' from Classical textual sources (which would require specialists trained in reconstruction) to living practice and institutional transmission (which democratizes participation). Ecclesiastical and scholarly communities gain autonomy in language use; Classical philologists lose the exclusive authority to declare forms correct or incorrect by appeal to ancient texts.
% ABSENT_VOICES: Later Renaissance humanists who would champion Classical purism and textual reconstruction are structurally excluded from the medieval period's self-understanding under this reading. They would contest the reading's core premise but are temporally outside its frame. Natural philosophers who might ask whether language 'really' evolves or 'really' degrades are bracketed by the reading's focus on practice rather than metaphysical claims.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished and only the discontinuity reading prevailed (Classical forms as sole legitimate standard), medieval scribes and practitioners would face constant pressure to correct their practice against ancient texts. This would require investing in Classical training, consulting texts, and revising usage—creating friction in the transmission chain and potentially fracturing the Church's linguistic unity. The medieval world would reorganize around either continuous effort to restore Classical norms or abandonment of unified Latin in favor of vernaculars.
% FOUNDING_PROBLEM: After Classical Latin's shift from vernacular to liturgical/scholarly language in late antiquity, practitioners encountered the practical problem: evolving linguistic forms across the early medieval period (sound changes in Romance-speaking regions, case-system pressures, vocabulary borrowing from substrate languages) created divergence from Classical texts. The founding problem was whether to treat this as legitimate evolution (continuity reading) or corrupt deviation (discontinuity reading).
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical authorities and scribal centers attest that medieval Latin forms diverged from Classical texts and that the question of legitimacy recurs whenever a new text is composed. Modern historical linguists external to the medieval tradition—Romance philologists, historical phonologists, sociolinguists studying language change—attest that the forms in question are consistent with natural phonetic and morphological evolution patterns observable across language families and historical periods.
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).

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
 *   Extractiveness is low-to-moderate (0.31 at interval end) because the constraint's operation moves authority away from a scarce Classical expertise (high-cost reconstruction) toward distributed institutional practice. This is coordinative rather than extractive: it enables broad participation in Latin literacy without requiring constant textual validation. However, extractiveness rises slightly over the interval (from 0.12 to 0.31) as the accumulation of medieval divergence becomes undeniable and the reading must increasingly assert that normal linguistic evolution is legitimate—the constraint begins to extract from the discontinuity reading's authority claims. Suppression is low (0.22 at interval end) because the continuity reading does not require coercion to maintain—practitioners are already doing what the reading vindicates. The small suppression reflects the mild friction from Classical philologists objecting to medieval forms, but this friction is largely absorbed by the reading's own logic rather than suppressed externally. Theater ratio is very low and stable (0.08–0.18) because the constraint is functionally adequate: it genuinely enables transmission and use of Latin without substantial performative overhead. The measurement series tracks the historical emergence of the reading as a coherent normative position (early medieval period: reading is implicit in practice; high medieval: reading becomes explicit in theoretical discussions of language correctness).
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical-institutional seat, the continuity reading appears as genuine coordination: it validates their practice and enables unified linguistic community across centuries without constant correction. From the Classical philologist's seat, the same constraint appears as a loss of authority and interpretive autonomy—they are no longer the exclusive arbiters of correctness. The engine should compute this divergence: the beneficiary seats (ecclesiastical, practitioners) see a rope; the payer seat (philologists) may see suppression of their authority claims. The architectural reason for divergence is directionality: the constraint's legitimacy mechanism (practice-based authority) directly contradicts the philologist's epistemology (text-based reconstruction), so they bear a high directional d (near target) while practitioners bear low d (near beneficiary).
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical institutions and medieval practitioners are structural beneficiaries (d near 0.0): the reading validates their existing practice and frees them from constant appeal to Classical texts for legitimacy. They have constrained exit (cannot easily switch to a different Latin authority structure without institutional friction), but they benefit sufficiently from the validation that exit pressure is low. Classical philologists are constrained payers (d near 0.7): they lose exclusive authority to declare forms correct, and their specialized expertise in Classical forms becomes less essential if medieval forms are legitimate on their own terms. However, their exit is constrained—they remain embedded in scholarly institutions that continue to use Latin. Renaissance humanists are effectively excluded (d undefined in this period) because they have not yet emerged as a coherent movement. Vernacular-speaking communities benefit from framing language evolution as legitimate (d near 0.2), but their benefit is diffuse and contingent on the reading becoming dominant. No override is needed: the structural derivation captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading avoids the mandatrophy collapse by remaining tied to actual practice throughout the medieval period. Its founding problem—the emergence of linguistic divergence from Classical forms—remains live because medieval practitioners continue to produce divergent forms, and the question 'are these legitimate?' recurs with each new text. The reading does not become theater because it continues to do real coordination work: it enables a unified Latin community across centuries without requiring constant textual expertise. The constraint's persistence is grounded in functional adequacy (practitioners can communicate and transmit via living practice) rather than performance maintenance. The measurement trajectory shows extractiveness plateauing around 1000 CE, suggesting the reading's normative position stabilizes once the divergence is clearly irreversible—at that point, the reading's assertion that evolution is legitimate becomes harder to contest empirically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practice_vs_text_authority,
    'When medieval practice and Classical texts diverge, which should adjudicate legitimacy: the coherence and functional adequacy of evolving practice, or the authority of ancient texts preserved in manuscript tradition?',
    'Historical-linguistic analysis of actual medieval writing practices (manuscript evidence, legal documents, liturgical texts) and the internal coherence of medieval Latin morphology and syntax; philosophical interrogation of what ''authority'' means in language (convention, historical precedent, functional adequacy, textual purity).',
    'If practice-coherence is the primary authority, the continuity reading is vindicated and medieval forms are legitimate evolution. If textual purity is primary, the discontinuity reading gains support and medieval forms are treated as deviations requiring correction. If both hold in different contexts (legal vs. poetic, ecclesiastical vs. secular), the hybrid reading becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_vs_text_authority, conceptual, 'What grounds legitimacy in language: actual use practice or recovered canonical texts?').

omega_variable(
    evolution_vs_corruption_framing,
    'Are the phonetic and morphological changes in medieval Latin natural linguistic evolution (analogous to documented sound change in living languages) or corruption of a purer Classical form?',
    'Comparative historical-linguistic study: do the changes in medieval Latin follow patterns observable in Romance language evolution from Latin substrates? Do they follow patterns of phonetic change, analogical leveling, and borrowing seen in other historical language corpora?',
    'If the changes follow natural evolution patterns, the continuity reading''s framing gains empirical support. If the changes appear random or violate constraints on linguistic evolution, the corruption framing (discontinuity reading) becomes more plausible. The hybrid reading can absorb either answer by distinguishing ''natural evolution'' (accept) from ''errors'' (correct).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evolution_vs_corruption_framing, empirical, 'Whether medieval Latin changes follow natural linguistic evolution or deviate from normal patterns.').

omega_variable(
    institutional_bias_in_transmission,
    'Does the Church''s preservation of medieval Latin texts and practices constitute a genuine transmission of living Latin, or does it preserve medieval Latin precisely because the Church has institutional incentive to validate its own evolved forms?',
    'Examination of non-ecclesiastical medieval Latin writing (legal documents, secular correspondence, commercial records) to see whether the evolved forms are genuinely widespread across institutions or concentrated in Church contexts; comparison with Romance language data to isolate Church-specific vs. universal changes.',
    'If medieval forms are universal across contexts, the continuity reading''s claim about living practice is stronger. If forms are Church-concentrated and Church-defended because of institutional interest, the reading becomes more extractive (institutional beneficiaries defending their practice against external scrutiny). If secular and Church Latin diverge, the reading''s claim about unified transmission is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_bias_in_transmission, empirical, 'Whether the Church''s preservation of medieval Latin reflects genuine living practice or institutional protection of evolved forms.').

omega_variable(
    committer_kernel_contest,
    'This constraint is ONE reading of a contested kernel about correct Latin. What is the status of the other readings (discontinuity_reading, hybrid_reading) relative to this reading''s core premises? Do they foreclose this reading''s legitimacy, coexist with it as live alternatives, or influence it by creating conditions where it operates differently?',
    'Historical-institutional analysis of which reading(s) different medieval parties actually held and how explicitly; examination of whether Renaissance humanists'' later embrace of discontinuity_reading involved explicit rejection of the continuity reading or merely different priorities; textual evidence from medieval scholars about whether they acknowledged the discontinuity_reading as a live alternative or were genuinely unaware of it.',
    'If the discontinuity reading was always a live alternative (ancient scholars knew both, medieval scholars debated both), the coexists_with relation holds and the readings are genuinely sibling frameworks. If the continuity reading was the only framework available to medieval practitioners and discontinuity emerged later, the influence relation is directional: Renaissance adoption of discontinuity_reading reinterprets medieval practice retroactively. If readings are mutually foreclosing (cannot hold both in any single framework), the kernel exhibits zero-sum competition rather than coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_contest, conceptual, 'The logical and historical relationship between the continuity reading and its sibling readings (discontinuity, hybrid).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 400, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t400, correct_latin__continuity_reading, theater_ratio, 400, 0.08).
narrative_ontology:measurement_basis(corr_tr_t400, projected).
narrative_ontology:measurement(corr_tr_t550, correct_latin__continuity_reading, theater_ratio, 550, 0.12).
narrative_ontology:measurement_basis(corr_tr_t550, observed).
narrative_ontology:measurement(corr_tr_t700, correct_latin__continuity_reading, theater_ratio, 700, 0.15).
narrative_ontology:measurement_basis(corr_tr_t700, observed).
narrative_ontology:measurement(corr_tr_t850, correct_latin__continuity_reading, theater_ratio, 850, 0.17).
narrative_ontology:measurement_basis(corr_tr_t850, observed).
narrative_ontology:measurement(corr_tr_t1000, correct_latin__continuity_reading, theater_ratio, 1000, 0.18).
narrative_ontology:measurement_basis(corr_tr_t1000, observed).
narrative_ontology:measurement(corr_tr_t1200, correct_latin__continuity_reading, theater_ratio, 1200, 0.18).
narrative_ontology:measurement_basis(corr_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t400, correct_latin__continuity_reading, base_extractiveness, 400, 0.12).
narrative_ontology:measurement_basis(corr_be_t400, projected).
narrative_ontology:measurement(corr_be_t550, correct_latin__continuity_reading, base_extractiveness, 550, 0.18).
narrative_ontology:measurement_basis(corr_be_t550, observed).
narrative_ontology:measurement(corr_be_t700, correct_latin__continuity_reading, base_extractiveness, 700, 0.24).
narrative_ontology:measurement_basis(corr_be_t700, observed).
narrative_ontology:measurement(corr_be_t850, correct_latin__continuity_reading, base_extractiveness, 850, 0.28).
narrative_ontology:measurement_basis(corr_be_t850, observed).
narrative_ontology:measurement(corr_be_t1000, correct_latin__continuity_reading, base_extractiveness, 1000, 0.3).
narrative_ontology:measurement_basis(corr_be_t1000, observed).
narrative_ontology:measurement(corr_be_t1200, correct_latin__continuity_reading, base_extractiveness, 1200, 0.31).
narrative_ontology:measurement_basis(corr_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t400, correct_latin__continuity_reading, suppression_requirement, 400, 0.08).
narrative_ontology:measurement_basis(corr_su_t400, projected).
narrative_ontology:measurement(corr_su_t550, correct_latin__continuity_reading, suppression_requirement, 550, 0.12).
narrative_ontology:measurement_basis(corr_su_t550, observed).
narrative_ontology:measurement(corr_su_t700, correct_latin__continuity_reading, suppression_requirement, 700, 0.16).
narrative_ontology:measurement_basis(corr_su_t700, observed).
narrative_ontology:measurement(corr_su_t850, correct_latin__continuity_reading, suppression_requirement, 850, 0.2).
narrative_ontology:measurement_basis(corr_su_t850, observed).
narrative_ontology:measurement(corr_su_t1000, correct_latin__continuity_reading, suppression_requirement, 1000, 0.22).
narrative_ontology:measurement_basis(corr_su_t1000, observed).
narrative_ontology:measurement(corr_su_t1200, correct_latin__continuity_reading, suppression_requirement, 1200, 0.22).
narrative_ontology:measurement_basis(corr_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin__continuity_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The kernel 'correct_latin' decomposes into three structurally distinct constraints corresponding to three readings of what constitutes legitimate Latin and who adjudicates it. The continuity_reading (this file) asserts practice-based authority. The discontinuity_reading asserts text-based authority. The hybrid_reading asserts mixed authority with targeted correction. Each reading instantiates a different beneficiary/victim structure, different extraction profile, different authority grounding (cs_structure.authority_grounding). The three constraints are linked by kernel membership: all three are readings of the same contested kernel. The discontinuity reading (classical_philologist-favored) influences this reading by creating pressure toward textual correction that the continuity reading must resist. This reading influences the hybrid reading by establishing that practice is legitimate; the hybrid reading then adds the refinement that texts can improve practice selectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
