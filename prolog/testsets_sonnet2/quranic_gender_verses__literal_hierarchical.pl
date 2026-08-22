% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Literal-Hierarchical Reading of Qur'anic Gender Verses (4:11, 2:282, 4:34)
 *   domain: Islamic Jurisprudence / Legal Hermeneutics / Gender Studies
 *
 * SUMMARY:
 *   This story instantiates the literal-hierarchical reading of a contested
 *   kernel: three Qur'anic verses (4:11 inheritance, 2:282 commercial-debt
 *   testimony, 4:34 marital guardianship) treated as direct, timeless,
 *   non-abrogable legal ordinances establishing differentiated inheritance
 *   shares, testimony weighting, and male marital authority. Under this
 *   reading the coordination function (a stable, textually-grounded rule set
 *   for family and commercial law) is real, but it is bundled with asymmetric
 *   extraction: male household heads, male heirs, and religious court
 *   authorities hold structural authority and resource control, while women
 *   bear constrained inheritance, discounted testimonial credibility, and
 *   restricted marital exit, enforced by religious courts that treat
 *   contextual or abrogationist alternatives as illegitimate. This is ONE of
 *   three linked readings of the same kernel (quranic_gender_verses); the
 *   contextual_egalitarian and progressive_abrogation readings are separate
 *   constraint stories with their own ε values, beneficiary/victim
 *   structures, and classifications, per the ε-invariance principle — this
 *   file does not describe or average over them.
 *
 * KEY AGENTS:
 *   - male_household_heads: Primary beneficiary and local agenda-setter (powerful/arbitrage) — holds guardianship authority and doubled inheritance
 *   - religious_court_authorities: Institutional agenda-setter (institutional/analytical) — codifies and enforces the literal reading as binding law
 *   - wives_under_guardianship: Primary target (powerless/trapped) — bears disciplinary authority, restricted autonomy, asymmetric divorce
 *   - female_heirs: Secondary target (moderate/constrained) — bears halved inheritance share
 *   - female_witnesses_in_commercial_disputes: Secondary target (moderate/constrained) — bears discounted testimonial weight
 *   - reformist_jurists: Excluded voice (organized/constrained) — advocates alternative readings without binding authority
 *   - comparative_legal_scholars: Analytical observer (analytical/analytical) — compares codification outcomes across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.78).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.81).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Literal-Hierarchical Reading of Qur'anic Gender Verses (4:11, 2:282, 4:34)").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "Islamic Jurisprudence / Legal Hermeneutics / Gender Studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, 'd832ea11-1408-49fa-8a09-bd44f8966961').
narrative_ontology:cs_kernel_codification('d832ea11-1408-49fa-8a09-bd44f8966961', fixed_text).
narrative_ontology:cs_authority_grounding('d832ea11-1408-49fa-8a09-bd44f8966961', lineage).
narrative_ontology:cs_interpretation_layer_present('d832ea11-1408-49fa-8a09-bd44f8966961').
narrative_ontology:cs_reading_relation('d832ea11-1408-49fa-8a09-bd44f8966961', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_reading_relation('d832ea11-1408-49fa-8a09-bd44f8966961', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('d832ea11-1408-49fa-8a09-bd44f8966961', foundational, textual_literalism_precludes_reinterpretation).
narrative_ontology:cs_axiom_status(textual_literalism_precludes_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('d832ea11-1408-49fa-8a09-bd44f8966961', textual_literalism_precludes_reinterpretation, theological).
narrative_ontology:cs_axiom('d832ea11-1408-49fa-8a09-bd44f8966961', foundational, male_qawama_as_permanent_ordinance).
narrative_ontology:cs_axiom_status(male_qawama_as_permanent_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('d832ea11-1408-49fa-8a09-bd44f8966961', male_qawama_as_permanent_ordinance, theological).
narrative_ontology:cs_reference_frame('d832ea11-1408-49fa-8a09-bd44f8966961', classical_tafsir_literal_transmission).
narrative_ontology:cs_drift_state('d832ea11-1408-49fa-8a09-bd44f8966961', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d832ea11-1408-49fa-8a09-bd44f8966961', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_heirs).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_court_authorities).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_witnesses_in_commercial_disputes).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_heirs).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, wives_under_guardianship).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_witnesses_in_commercial_disputes).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_seeking_marital_exit).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, divine_textual_immutability).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, qawwamun_male_guardianship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold formal guardianship (qawama) over wives and dependents under this reading of 4:34, controlling household finances, travel permission, and disciplinary authority. Their inheritance share under 4:11 is double a comparable female heir's. They administer family affairs and can invoke religious-court backing when guardianship is contested; their exit from the arrangement costs them nothing since it is the seat of their authority.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter).

% Adjudicate inheritance division, testimony admissibility, and guardianship disputes by applying the literal reading of these three verses as settled, non-negotiable law. They enforce the doubled male inheritance share, the two-women-to-one-man testimony rule in financial contracts, and the husband's disciplinary and authority prerogatives under 4:34. Their institutional legitimacy rests on treating the verses as timeless divine ordinance rather than historically contingent guidance.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_court_authorities, agenda_setter,
    institutional, civilizational, analytical, national).

% Receive half the inheritance share of an equivalently positioned male relative under the literal application of 4:11. They can contest allocations only within the same religious court system that enforces the differential; appeal to secular courts is unavailable in jurisdictions applying this reading as codified family and inheritance law.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_heirs, payer,
    moderate, biographical, constrained, national).

% Live under a husband's guardianship authority as codified from 4:34, including his prerogative to restrict movement, control shared finances, and exercise graduated disciplinary measures culminating in physical correction as sanctioned in the literal reading. Divorce initiation is procedurally harder for them than for husbands, and social and legal exit costs include loss of custody presumption, community standing, and economic support.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, wives_under_guardianship, payer,
    powerless, biographical, trapped, national).

% Under the literal application of 2:282's commercial-debt testimony rule, their individual testimony in financial contract disputes is weighted at half a man's, requiring a second female witness to corroborate. This reduces their standing as sole witnesses in business dealings and shapes whether they are sought as contracting parties at all.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_witnesses_in_commercial_disputes, payer,
    moderate, biographical, constrained, national).

% Face asymmetric divorce procedures relative to husbands' unilateral talaq rights under the guardianship framework this reading upholds; formal apostasy or open rejection of the doctrine risks family rupture, loss of children, and community exclusion, making exit from the arrangement itself, not merely from a given marriage, extremely costly.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_seeking_marital_exit, payer,
    powerless, biographical, trapped, national).

% Argue for contextual or abrogation-based readings that would revise inheritance, testimony, and guardianship rules, but are excluded from binding authority in jurisdictions where the literal-hierarchical reading is state-codified; their scholarly output circulates but does not alter enforced law without political or judicial adoption.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, reformist_jurists, excluded,
    organized, generational, constrained, national).

% Study how the same three verses are codified differently across jurisdictions, comparing outcomes under literal, contextual, and abrogationist readings without themselves being subject to the arrangement's enforcement.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, diffuse).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, judicially administrable rule set for inheritance division, contractual testimony weighting, and household authority, removing case-by-case negotiation and grounding family and commercial law in a stable, court-enforceable textual source.
% TRANSFER_FUNCTION: Moves inheritance share, financial and legal authority, testimonial credibility, and marital decision-making power from female family members and female commercial witnesses to male heirs, husbands, and male witnesses, formalized through religious court enforcement.
% ABSENT_VOICES: Reformist jurists advocating contextual or abrogation-based readings are structurally outside the enforcing courts' binding authority in jurisdictions that codify the literal reading; women themselves are rarely seated as judges or muftis in the courts applying these rules to their own cases.
% DISAPPEARANCE_RATIONALE: If this specific reading's legal codification disappeared, inheritance shares, testimony weighting, and marital guardianship authority would have to be renegotiated under an alternative framework (contextual, abrogationist, or secular civil code); male household heads and religious courts administering this reading would lose the specific authority and resource allocation this reading currently guarantees them.
% FOUNDING_PROBLEM: Seventh-century Arabian legal and economic conditions in which men bore near-exclusive financial responsibility for dependents, extended family, and mahr (dower) obligations, and in which formal literacy and commercial experience were concentrated among men, creating a claimed rationale for differentiated inheritance, testimony, and guardianship rules.
% FOUNDING_PROBLEM_CORROBORATION: Religious court authorities and male household heads applying this reading attest the underlying rationale (male financial responsibility, differentiated capacity) remains live and binding as divine ordinance independent of changed material conditions. Comparative legal scholars and reformist jurists, from outside the beneficiary set, attest that the material preconditions cited in the founding rationale (exclusive male financial responsibility, gendered literacy gaps) have substantially changed in most contemporary contexts where this reading is still enforced, while the legal codification has not correspondingly adjusted.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) because the reading's operation transfers concrete, quantifiable resources and authority (inheritance share, testimonial credibility, marital decision rights) from women to men through the same textual mechanism claimed to coordinate family and commercial law. Suppression is authored comparably high (0.81) because persistence depends on courts treating the literal reading as the only legitimate one, foreclosing the contextual and abrogationist alternatives that coexist as live scholarly positions but lack enforcement standing in jurisdictions applying this reading. Accessibility collapse is high (0.7): once codified as state or communal family law, alternative readings are not available as a practical legal remedy for someone inside the system. Resistance is substantial but not overwhelming (0.62): reformist jurists, women's rights movements, and comparative legal critique actively contest the reading, but without judicial or political power to alter enforced codification. Theater ratio is modest (0.22) and rises slowly — the enforcement machinery is doing real distributive work, not primarily performing; the rise reflects growing use of procedural and rhetorical defense of the doctrine ('preserving tradition') as external legal-reform pressure increases over the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads and religious court authorities sit near the full-beneficiary end: they collect authority, resources, and legitimacy directly from the arrangement and can invoke it at will. Wives under guardianship sit near the full-target end with trapped exit — apostasy, custody loss, and social rupture make leaving the framework itself, not just a marriage, extremely costly, which the engine should read as amplifying effective extraction beyond the base derivation. Female heirs and female witnesses are targets with constrained (not fully trapped) exit, since inheritance and commercial disputes admit some procedural contestation within the same court system, even if the underlying rule is not revisable there. Reformist jurists are excluded rather than coordinated or extracted from directly — their exclusion is what keeps the enforcement structure uncontested at the level that matters.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (male-concentrated financial responsibility and commercial literacy in 7th-century Arabia) is authored as contested rather than flatly dead: the literal-hierarchical reading's own proponents hold the rationale as timeless and independent of material conditions, while outside corroboration (comparative legal scholarship) documents that the material preconditions have substantially changed in most contemporary contexts where the reading is still enforced. This divergence is exactly the kind of status the R5 corroboration field exists to surface — the doctrine is not self-evidently mandatrophic (the reading rejects that framing entirely) nor self-evidently still functionally necessary; classification of this reading as tangled_rope rather than pure snare tracks the fact that a genuine historical coordination problem existed and the courts' interpretive apparatus (case precedent, procedural regularity) is real infrastructure, not empty performance — but the persistence of asymmetric extraction after the cited material conditions changed is the mandatrophy signal an abolition-minded reading would flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_reading_committer_structure,
    'Is the literal-hierarchical reading of these three verses the kernel''s genuinely singular, intended meaning, or one of several live readings competing for interpretive authority within the same textual tradition?',
    'This is not resolvable by new evidence within this reading''s own framework — it is a hermeneutic commitment. Sibling constraint stories (contextual_egalitarian, progressive_abrogation) exist precisely because different interpretive communities hold this ambiguity differently and produce structurally distinct constraints from the same verses.',
    'If a jurisdiction''s courts or a community''s religious authority shift interpretive commitment toward a sibling reading, the entire beneficiary/victim structure and extraction profile authored here would no longer describe the operative constraint in that jurisdiction — the population would migrate to a differently-classified constraint story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literal_reading_committer_structure, conceptual, 'Whether the literal-hierarchical reading is the kernel''s sole legitimate reading or one contested reading among several.').

omega_variable(
    founding_rationale_material_persistence,
    'Do the material conditions cited as this reading''s founding rationale (concentrated male financial responsibility, gendered commercial literacy gaps) still hold in the specific jurisdictions where this reading remains codified as binding law?',
    'Jurisdiction-by-jurisdiction empirical study of female labor force participation, financial literacy, and independent economic responsibility compared against the reading''s stated rationale.',
    'Where material conditions have changed but the codification has not, the founding_problem_status divergence documented in six_questions strengthens the mandatrophy read; where conditions genuinely persist in a given locale, the coordination-function claim is comparatively stronger there.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_rationale_material_persistence, empirical, 'Whether the founding rationale''s material preconditions still hold where the reading is enforced.').

omega_variable(
    exit_cost_attribution,
    'Is the high exit cost for women under this reading (apostasy risk, custody loss, community rupture) attributable to the doctrinal content of the reading itself, or to surrounding social and state enforcement structures (family courts, custody law, social stigma) that could in principle be decoupled from the interpretive question?',
    'Comparative study of communities that hold this doctrinal reading privately/theologically but do not codify it into enforceable family law, versus jurisdictions where it is state-codified.',
    'If exit costs are primarily attributable to state codification rather than the doctrine itself, the constraint''s tangled_rope/snare severity is a property of the legal-enforcement layer, not the interpretive layer — suggesting the suppression metric should be partitioned between doctrinal and civil-enforcement components in future decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_attribution, empirical, 'Whether trapped exit options stem from doctrinal content or from separable state/social enforcement infrastructure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t8, quranic_gender_verses__literal_hierarchical, theater_ratio, 8, 0.13).
narrative_ontology:measurement(qura_tr_t16, quranic_gender_verses__literal_hierarchical, theater_ratio, 16, 0.16).
narrative_ontology:measurement(qura_tr_t24, quranic_gender_verses__literal_hierarchical, theater_ratio, 24, 0.18).
narrative_ontology:measurement(qura_tr_t32, quranic_gender_verses__literal_hierarchical, theater_ratio, 32, 0.2).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__literal_hierarchical, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(qura_be_t8, quranic_gender_verses__literal_hierarchical, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(qura_be_t16, quranic_gender_verses__literal_hierarchical, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(qura_be_t24, quranic_gender_verses__literal_hierarchical, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(qura_be_t32, quranic_gender_verses__literal_hierarchical, base_extractiveness, 32, 0.77).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__literal_hierarchical, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(qura_su_t8, quranic_gender_verses__literal_hierarchical, suppression_requirement, 8, 0.71).
narrative_ontology:measurement(qura_su_t16, quranic_gender_verses__literal_hierarchical, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(qura_su_t24, quranic_gender_verses__literal_hierarchical, suppression_requirement, 24, 0.77).
narrative_ontology:measurement(qura_su_t32, quranic_gender_verses__literal_hierarchical, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__literal_hierarchical, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, enforcement_mechanism).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quranic_gender_verses kernel. quranic_gender_verses__contextual_egalitarian reads the same verses as historically-situated progressive measures requiring reinterpretation under maqasid equity principles (expected much lower ε, rope/scaffold-leaning). quranic_gender_verses__progressive_abrogation reads them as an incomplete trajectory superseded by later universalist verses via naskh (expected transitional, decaying-extraction profile, scaffold/piton-leaning). Each story authors its own ε, beneficiary/victim structure, and classification independently per the ε-invariance principle; they are linked here for contamination-propagation and comparative analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
