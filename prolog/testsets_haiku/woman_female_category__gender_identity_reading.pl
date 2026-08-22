% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Gender Identity as Woman/Female Category Boundary (Identity-Based Reading)
 *   domain: political/bioethics/law
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of a contested kernel: what
 *   makes someone a member of the 'woman' or 'female' category? The
 *   gender-identity reading asserts that internal self-identification with
 *   the gender category woman/female is the primary and legitimate boundary,
 *   independent of biological sex. This reading benefits transgender
 *   individuals seeking institutional recognition and identity-aligned
 *   access; it extracts from cisgender women who experience the boundary
 *   redefinition as dissolution of sex-based coordination, and from
 *   sex-category essentialists who view sex biology as the legitimate
 *   boundary. The constraint is CLAIMED as tangled_rope (genuine coordination
 *   function + asymmetric extraction + active enforcement) and the authored
 *   metrics describe substantially extractive operation with rising
 *   institutional enforcement. The kernel contest involves two sibling
 *   readings: the sex-biology reading (chromosomal/reproductive sex as
 *   boundary) and the hybrid-contextual reading (context-dependent
 *   boundaries). Each reading instantiates a different constraint with
 *   different ε, different beneficiaries/victims, different extraction logic.
 *
 * KEY AGENTS:
 *   - Transgender women: seek identity-based category recognition; trapped by identity-lock; powerless institutional position initially, increasingly organized
 *   - Cisgender women in contested spaces: experience boundary redefinition; constrained exit; moderate power; bear forced participation in category redefinition
 *   - Sex-category essentialists: defend reproductive-sex boundary; powerful institutional position (some); constrained by institutional drift toward identity framework
 *   - Gender-identity advocates: set institutional policy; organized; agenda-setting power; benefit from framework consolidation
 *   - Institutional managers: navigate competing category claims; bear operational burden; observer seat on category legitimacy question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.68).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.72).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity as Woman/Female Category Boundary (Identity-Based Reading)").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political/bioethics/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '8c5a3be3-7cca-4439-873a-b3b9505d195d').
narrative_ontology:cs_kernel_codification('8c5a3be3-7cca-4439-873a-b3b9505d195d', distributed).
narrative_ontology:cs_authority_grounding('8c5a3be3-7cca-4439-873a-b3b9505d195d', extraction).
narrative_ontology:cs_interpretation_layer_present('8c5a3be3-7cca-4439-873a-b3b9505d195d').
narrative_ontology:cs_reading_relation('8c5a3be3-7cca-4439-873a-b3b9505d195d', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c5a3be3-7cca-4439-873a-b3b9505d195d', woman_female_category__hybrid_contextual_reading, coexists_with).
narrative_ontology:cs_axiom('8c5a3be3-7cca-4439-873a-b3b9505d195d', foundational, gender_identity_constitutes_woman_category).
narrative_ontology:cs_axiom_status(gender_identity_constitutes_woman_category, holdable).
narrative_ontology:cs_axiom_grounding('8c5a3be3-7cca-4439-873a-b3b9505d195d', gender_identity_constitutes_woman_category, deontological).
narrative_ontology:cs_axiom('8c5a3be3-7cca-4439-873a-b3b9505d195d', foundational, institutional_recognition_of_gender_identity_required).
narrative_ontology:cs_axiom_status(institutional_recognition_of_gender_identity_required, holdable).
narrative_ontology:cs_axiom_grounding('8c5a3be3-7cca-4439-873a-b3b9505d195d', institutional_recognition_of_gender_identity_required, deontological).
narrative_ontology:cs_reference_frame('8c5a3be3-7cca-4439-873a-b3b9505d195d', institutional_non_recognition_of_trans_identity).
narrative_ontology:cs_drift_state('8c5a3be3-7cca-4439-873a-b3b9505d195d', contemporary_trans_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8c5a3be3-7cca-4439-873a-b3b9505d195d', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, cisgender_women_in_contested_spaces).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, sex_category_essentialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, biological_sex_essentialists).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, women_in_sex_segregated_professions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek social, legal, and institutional recognition of woman/female identity aligned with their internal gender self-identification. Rely on identity-based category membership for access to women's spaces (bathrooms, shelters, sports, prisons), legal documentation, healthcare protocols that align with gender identity, and social dignity. Their identity is constitutively linked to the category claim; exit would require denying fundamental self-understanding.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_women, beneficiary,
    powerless, biographical, identity_locked, national).

% Experience redefinition of the category 'woman' they understood as coterminous with female sex. In spaces where sex-segregation was premised on reproductive anatomy, developmental biology, or historical sex-class experience (domestic violence shelters, women's prisons, changing facilities, athletic competitions, domestic-violence or sexual-assault support groups), they now share access with individuals whose sex category may differ from their own. They perceive this as dissolution of sex-based coordination and forced participation in boundary-dilution they did not consent to. Exit options are constrained: creating alternative women's spaces is costly and organizationally difficult; using existing spaces means encountering the boundary-membership change.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, cisgender_women_in_contested_spaces, payer,
    moderate, biographical, constrained, national).

% Advocate that 'woman' and 'female' denote reproductive sex categories whose boundaries are set by biological reality, not identity declaration. They see the gender-identity reading as a category collapse that erases sex-based analysis, eliminates sex-segregated spaces that coordinate for sex-specific harms (reproductive coercion, menstrual dysphoria, pregnancy-related discrimination), and transfers decision-making over sex-category boundaries from biology (observable, stable, defensible) to internal states (variable, undisprovable, contestable). Their exit is constrained: engaging in politics and speech to restore sex-based legal categories, but facing institutional and legal pressure to recognize gender identity as the primary category boundary.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, sex_category_essentialists, payer,
    powerful, generational, constrained, national).

% Institutional and political actors (civil rights organizations, progressive legal scholars, administrative bodies, some medical authorities) that have adopted gender identity as the legitimate category boundary and actively enforce this reading through legal reform, institutional policy, healthcare guidelines, and public discourse. They set the terms of membership, define which spaces count as 'women's spaces,' shape institutional recognition, and deploy social sanction against dissent. They benefit from institutional authority and the consolidation of gender-identity framework across multiple domains.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, gender_identity_advocates, beneficiary).

% Institutional actors (some medical bodies, athletic organizations, conservative legal scholars, religious institutions) that defend sex-based categorization and resist gender-identity framework adoption. They attempt to maintain separate category systems: sex categories for medicine/sports/law, identity categories for social respect. They face institutional and legal pressure and operate under conditions where their framework is labeled discriminatory; their exit is constrained by institutional sunk costs and ideological commitment.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, biological_sex_essentialists, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, biological_sex_essentialists, agenda_setter).

% Individuals (prison staff, shelter workers, medical professionals, sports administrators) who manage sex-segregated institutions or services and must now navigate redefined category boundaries. They face enforcement pressure: institutions mandate gender-identity recognition; resistance or continued sex-based admission triggers legal liability and social sanction. They bear the operational cost of managing competing constituency claims (trans women seeking inclusion, cisgender women seeking sex-segregated safety, liability exposure to both groups).
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, women_in_sex_segregated_professions, payer,
    moderate, biographical, constrained, national).

% Political movement claiming sex category as the legitimate basis for women's legal protections and organizing space. Excluded from institutional decision-making about category boundaries; their positions are framed as discriminatory and transphobic; they lack institutional access to reshape policy in their direction. Their exclusion is maintained through labeling and institutional barriers, not through explicit prohibition.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, sex_based_rights_movement, excluded,
    moderate, generational, constrained, national).

% Track disease patterns, reproductive health outcomes, and healthcare disparities that may be sex-differentiated. Face pressure to adopt gender-identity category for official statistics and medical practice (hormone therapy protocols, cancer screening, pregnancy care) while maintaining sex-disaggregated data collection for epidemiology. Navigate the tension between administrative category standardization (gender identity) and clinical reality (sex differences in disease, medication response, reproductive function). Their analytical seat allows observation of how category boundary changes affect different constituencies.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__gender_identity_reading, gender_identity_advocates).
narrative_ontology:fixing_cost_class(woman_female_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social recognition, institutional access, and legal status around a definition of who counts as 'woman' and 'female.' The gender-identity reading solves the problem: how should institutional boundaries be drawn when internal gender identity diverges from sex category assignment? It offers a unified answer — use gender identity — that simplifies institutional decision-making, reduces administrative complexity of tracking multiple category systems, and ensures that individuals' self-understanding receives institutional recognition.
% TRANSFER_FUNCTION: Transfers authority to define category boundaries from biology/medical observation to internal self-identification. Moves access to sex-segregated spaces from sex-based eligibility to identity-based eligibility. Transfers social legitimacy from sex-essentialist frameworks to gender-identity frameworks. Transfers liability and enforcement burden to institutional managers who must navigate competing category claims. Moves cultural authority from sex-based women's movement language to gender-identity-inclusive language (some cisgender women experience this as delegitimization of sex-based analysis).
% ABSENT_VOICES: Sex-based rights advocates and feminist theorists who ground women's category and rights claims in reproductive sex are structurally excluded from institutional decision-making about category boundaries. Their positions are pre-labeled 'transphobic' and prohibited from policy deliberation in many institutional contexts. They would object that the reading erases sex-based harms and coordinate capacity, but their objection is not seated at the decision table.
% DISAPPEARANCE_RATIONALE: If gender-identity-based category membership disappeared and sex-based category membership was reinstated as the legal and institutional standard, institutional architecture would reorganize: sex-segregated spaces would revert to sex-based access; legal documents would track sex category again; medical protocols would sex-disaggregate; institutional inclusion/exclusion decisions would shift. Transgender individuals would lose institutional recognition of gender identity as the primary category; they would face legal barriers to access, healthcare protocols misaligned with their identity, and institutional non-recognition. Institutions would no longer expend enforcement effort on gender-identity recognition. The arrangement is not inevitable; it is the product of active institutional choice and legal reform.
% FOUNDING_PROBLEM: Individuals whose gender identity diverges from their assigned sex category at birth face institutional non-recognition, legal barriers to documentation aligned with their identity, exclusion from services and spaces organized around their gender identity, and medical protocols misaligned with their lived experience. The founding problem: how should institutions respond when individuals claim category membership (woman/female) that diverges from their birth sex assignment?
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocates and medical organizations (American Psychological Association, American Medical Association) attest the founding problem is live and identity-based recognition is necessary for mental health and dignity. Sex-based rights advocates and some medical organizations attest the problem is either overstated (most trans people can navigate existing sex-based systems) or is insoluble without erasing sex-based category entirely. Public health researchers note the founding problem is real but contested: some attest it requires identity-based recognition; others attest it requires gender-affirming care within sex-segregated systems rather than category redefinition. The corroboration is split along advocacy lines; no neutral external authority attests uncontestably to the founding problem's status.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.42 at t=0) because the reading solves a genuine coordination problem — institutional recognition of gender identity — but also imposes real costs on cisgender women and sex-essentialists (exclusion from decision-making, category redefinition without consent, loss of sex-based institutional spaces). It rises to 0.68 by t=25 as institutional enforcement hardens and sex-based alternatives are progressively eliminated. Suppression is substantial and rising (0.51 to 0.72): the constraint's persistence depends on active institutional suppression of sex-category framing and exclusion of sex-based objections from deliberation. Theater is low (0.28): while the constraint includes genuine coordination (identity recognition), the rising enforcement burden reflects increasing difficulty maintaining the reading without coercive institutional machinery. The measurement series on one shared time grid documents the constraint's trajectory from contested innovation to institutionally enforced doctrine. The time points capture the expansion phase: institutional adoption (t=0-5), legal standardization (t=5-15), and institutional hardening (t=15-25).
 *
 * PERSPECTIVAL GAP:
 *   From the transgender-beneficiary seat, the constraint is necessary coordination: institutional recognition of identity. From the cisgender-women seat, it is forced participation in boundary dissolution. From the sex-essentialist seat, it is institutional capture of category meaning. From the gender-identity-advocate seat, it is progressive alignment with justice. These divergent readings are NOT perceptual differences; they are structural: the seats have different exit options, different power, different gains/losses. The engine computes per-seat classifications from these structural differences. The constraint is a tangled rope from every seat simultaneously: genuine coordination (identity recognition is a real problem) + extraction (cisgender women and essentialists bear the cost of boundary redefinition, pay in exclusion and institutional non-recognition of their category frame). The perspectival gap is the central signal the tangled-rope classification detects.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women are structural beneficiaries (d near 0.1-0.2): they gain institutional recognition, access to identity-aligned spaces and services, legal documentation alignment; they are identity-locked (cannot exit the claim without denying self-understanding). Cisgender women in contested spaces are targets (d near 0.7-0.8): they bear the cost of boundary redefinition, constrained exit (leaving existing spaces is costly; staying means encountering the change), moderate power (can organize alternative spaces but face institutional and legal pressure). Sex-essentialists are targets (d near 0.6-0.7): they lose institutional authority to define category, constrained exit (defending sex-based categories now attracts legal liability and social sanction). Gender-identity advocates are beneficiaries (d near 0.2): they gain institutional authority, legal/policy power, cultural legitimacy. The directional spread reflects real structural asymmetry: those claiming the new category boundary (trans women, advocates) have high exit costs from that claim (identity-locked) and gain recognition; those defending the old boundary face institutional pressure and exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status is contested: transgender advocates attest the founding problem (institutional non-recognition of trans identity) is live and urgent; sex-essentialists attest it is either overstated or unsolvable without erasing sex-based analysis. The disappearance verdict is world_rearranges: if gender-identity-based category membership disappeared, institutions would reorganize around sex-based boundaries, affecting access to services and legal recognition. The mismatch signal is conditional: IF the founding problem's status shifts to 'dead' (institutional recognition becomes normalized and non-contested) while disappearance verdict stays 'world_rearranges', the constraint would flag as zombie — a real coordinating function that has become institutionally mandatory rather than solving a live problem. Currently the founding problem is contested and the verdict matches (contested + rearranges = contested-coordination, not mandatrophy). The rising suppression/enforcement series signals that institutional hardening is substituting for consensus: as the founding problem's status becomes more contested, enforcement intensity rises to maintain boundary adherence. This trajectory is consistent with institutional capture of category meaning — the constraint could be moving from tangled_rope (genuine coordination + extraction) toward snare (pure enforcement maintenance of a boundary essentialists reject). The engine will track this drift as new measurement points are added.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_boundary_enforcement_asymmetry,
    'Is the rising suppression/enforcement requirement a sign that the gender-identity reading is being maintained by coercive institutional machinery rather than evolving consensus, and does this signal the constraint is drifting from tangled_rope toward snare?',
    'Longitudinal tracking: if enforcement continues to rise while cisgender-women and essentialist dissent remains strong and organized, the constraint is accumulating extractive machinery. If enforcement plateaus and dissent fades (genuine consensus), the constraint has stabilized as sustainable coordination. If enforcement rises while suppression of dissent intensifies, the snare classification becomes more probable.',
    'If the constraint is drifting snare-ward, the type claim (tangled_rope) is structurally wrong; the engine would compute snare classification from future measurement points. This would signal institutional capture of category meaning rather than genuine multi-party coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_boundary_enforcement_asymmetry, empirical, 'Whether rising enforcement indicates institutional capture of the kernel or natural stabilization of genuine coordination.').

omega_variable(
    identity_locked_vs_constrained_boundary,
    'Is the transgender-beneficiary exit truly ''identity-locked'' (denying the claim would require denying self-understanding) or is it more accurately ''constrained'' (high personal cost, but logically possible)?',
    'Phenomenological and existential assessment: interview data from trans individuals about whether exiting the claim is psychologically impossible or merely costly. Philosophical analysis of whether identity-fusion is constitutive or contingent.',
    'If identity-locking is genuine, directionality for trans beneficiaries is at the low end (0.1-0.2, full beneficiary). If it is constrained exit, directionality rises to 0.35-0.45. This shifts the computed per-seat classification: from trans seats, the constraint might compute as rope (genuine coordination without extraction) if truly identity-locked, versus tangled_rope if merely constrained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_vs_constrained_boundary, conceptual, 'Whether trans identity is constitutively fused with the category claim or instrumentally dependent on it.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression structural (institutional barriers, legal penalties, exclusion from decision-making) or internalized (self-censorship, identity-fusion with the gender-identity framework, internalized delegitimation of sex-essentialist critique)?',
    'Post-exit suppression trajectory: if cisgender women or essentialists exit contested spaces, does suppression persist (internalized) or dissipate (structural)? Institutional resistance evidence: do institutional agents suppress dissent actively (structural) or do dissenting agents self-suppress (internalized)?',
    'If suppression is structural, the engine''s extraction computation reflects institutional coercion accurately. If internalized, the true suppression is higher than the measured structural suppression — the targets have absorbed the constraint''s logic and suppress themselves. This would increase the extracted-harm assessment and strengthen the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression is structural institutional machinery or internalized target behavior.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the gender-identity reading logically foreclose the sex-biology reading (can both be true within a single coherent framework), or do they coexist as incompatible but simultaneously-held commitments by different parties?',
    'Logical analysis: if someone asserts ''gender identity determines category membership AND biological sex determines category membership,'' is this incoherent (foreclosure) or merely contradictory across parties (coexistence)? Empirical observation: do contemporary institutions hold both frameworks simultaneously (coexistence) or enforce one exclusively (foreclosure)? Can the readings be integrated in a consistent logic (hybrid reading) or are they fundamentally opposed?',
    'If foreclosure is correct, the cs_structure.reading_relations should declare ''forecloses'' between gender-identity and sex-biology. If coexistence is correct, it should declare ''coexists_with''. If hybrid-contextual reading genuinely integrates them, both readings influence the hybrid rather than foreclosing each other. The classification of the reading-relation affects how the engine models kernel instability and reading drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether gender-identity and sex-biology readings are logically incompatible or empirically coexistent.').

omega_variable(
    foundational_problem_scope_contest,
    'Is the founding problem ''institutional non-recognition of transgender identity'' (gender-identity advocates'' frame) or is it ''institutional authority over category meaning given contested definitions'' (a meta-problem about how to resolve the kernel)?',
    'Genealogical investigation: what was the historical problem that motivated the institutional adoption of gender-identity recognition? Was it trans advocacy (institutional non-recognition was the problem) or broader gender-equality policy (unification of category frameworks was the problem)? Who defined the problem and whose problem was it originally?',
    'If the founding problem is institutional non-recognition, it remains live and status is ''live'' — the reading is solving an ongoing problem. If the founding problem is broader category-unification or institutional simplification, the status could shift to ''dead'' (solved) or ''resolved by side effect'' — the gender-identity reading achieved simplification but may have solved it incompletely or created new problems. The founding-problem-status x disappearance-verdict mismatch would then flag mandatrophy risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_scope_contest, empirical, 'Whether the founding problem is trans-specific non-recognition or institutional category standardization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__gender_identity_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__gender_identity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__gender_identity_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(woma_tr_t25, woman_female_category__gender_identity_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(woma_be_t5, woman_female_category__gender_identity_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(woma_be_t10, woman_female_category__gender_identity_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(woma_be_t15, woman_female_category__gender_identity_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(woma_be_t25, woman_female_category__gender_identity_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.51).
narrative_ontology:measurement(woma_su_t5, woman_female_category__gender_identity_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(woma_su_t10, woman_female_category__gender_identity_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(woma_su_t15, woman_female_category__gender_identity_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(woma_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(woma_su_t25, woman_female_category__gender_identity_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__gender_identity_reading, 0.12).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% woman_female_category is a contested kernel with three structurally distinct readings. The gender-identity reading (this constraint) asserts gender identity as the primary boundary; it has high ε on identity-recognition gains and extraction on boundary-redefinition costs. The sex-biology reading asserts reproductive sex as the boundary; it has negligible ε on biological-fact coordination and high extraction on trans-identity costs. The hybrid-contextual reading splits the difference — different boundaries for different domains. All three readings share the same kernel but diverge on how to resolve the boundary question. They are linked via network.affects_constraints because changes to the institutional salience of one reading affect the others' operative scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
