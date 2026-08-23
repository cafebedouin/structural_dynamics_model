% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive Originalist Reading of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   The restrictive originalist reading of the equality clause holds that
 *   'equality' in the 18th-century constitutional framework applied
 *   exclusively to propertied white males as political actors within the
 *   social contract tradition. This reading treats the Constitution's
 *   equality commitments as fixed at the founding: the franchise, legal
 *   personhood, and political rights extend only to those who occupied the
 *   sovereign position in 1787. All other groups — women, enslaved people,
 *   non-propertied men, indigenous peoples — are structurally excluded; their
 *   inclusion requires formal constitutional amendment, not judicial
 *   reinterpretation. The reading presents itself as faithful to the fixed
 *   meaning of the text (mountain claim), but operates as a constraint that
 *   actively maintains a narrow beneficiary set through judicial doctrine and
 *   interpretive methodology.
 *
 * KEY AGENTS:
 *   - propertied_white_males: Primary beneficiary (powerful/identity_locked) — receives full equality protections
 *   - women: Primary victim (powerless/trapped) — excluded from political equality
 *   - enslaved_people: Primary victim (powerless/trapped) — excluded from personhood and rights
 *   - non_propertied_white_males: Victim (moderate/constrained) — partial inclusion, property qualification barriers
 *   - indigenous_peoples: Victim (powerless/trapped) — excluded from the social contract entirely
 *   - originalist_judges: Agenda setter (institutional/analytical) — administers/enforces the reading
 *   - originalist_scholars: Beneficiary (organized/identity_locked) — interpretive framework validated
 *   - expansive_universalist_advocates: Excluded (organized/trapped) — would object to narrow scope
 *   - progressive_textualist_advocates: Excluded (organized/constrained) — would object to high amendment threshold
 *   - constitutional_scholars: Observer (analytical/analytical) — analyzes full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.78).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.85).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.78).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive Originalist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '87074fc3-e396-4303-8edc-92218f2d10d9').
narrative_ontology:cs_kernel_codification('87074fc3-e396-4303-8edc-92218f2d10d9', formalized).
narrative_ontology:cs_authority_grounding('87074fc3-e396-4303-8edc-92218f2d10d9', lineage).
narrative_ontology:cs_interpretation_layer_present('87074fc3-e396-4303-8edc-92218f2d10d9').
narrative_ontology:cs_reading_relation('87074fc3-e396-4303-8edc-92218f2d10d9', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('87074fc3-e396-4303-8edc-92218f2d10d9', equality_clause_scope__progressive_textualist, influences).
narrative_ontology:cs_axiom('87074fc3-e396-4303-8edc-92218f2d10d9', foundational, equality_scope_fixed_at_founding).
narrative_ontology:cs_axiom_status(equality_scope_fixed_at_founding, holdable).
narrative_ontology:cs_axiom_grounding('87074fc3-e396-4303-8edc-92218f2d10d9', equality_scope_fixed_at_founding, conventional).
narrative_ontology:cs_axiom('87074fc3-e396-4303-8edc-92218f2d10d9', secondary, amendment_as_exclusive_expansion_path).
narrative_ontology:cs_axiom_status(amendment_as_exclusive_expansion_path, holdable).
narrative_ontology:cs_axiom_grounding('87074fc3-e396-4303-8edc-92218f2d10d9', amendment_as_exclusive_expansion_path, conventional).
narrative_ontology:cs_reference_frame('87074fc3-e396-4303-8edc-92218f2d10d9', founding_social_contract_1787).
narrative_ontology:cs_drift_state('87074fc3-e396-4303-8edc-92218f2d10d9', contemporary_originalist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('87074fc3-e396-4303-8edc-92218f2d10d9', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_males).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, originalist_scholars).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_people).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_propertied_white_males).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, indigenous_peoples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, originalist_judges).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, originalist_interpretive_methodology).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, fixed_constitutional_meaning_at_founding).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, social_contract_theory_as_constitutional_foundation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold full political equality, suffrage, legal personhood, and property rights under the 1787 framework. Their identity as sovereign political actors is fused with the constitutional order — exit means abandoning the only framework that recognizes their full status. They benefit from the exclusion of others without directly administering the constraint.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_males, beneficiary,
    powerful, biographical, identity_locked, national).

% Excluded from political equality, suffrage, and full legal personhood. The constraint denies them citizenship rights coextensive with men. Exit is structurally blocked — they cannot leave the constitutional order, and the amendment threshold (Article V) requires supermajorities they cannot achieve without the constraint's beneficiaries consenting. Their subordination is maintained through coverture, lack of suffrage, and denial of office-holding.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women, payer,
    powerless, generational, trapped, national).

% Denied legal personhood entirely — treated as property, not parties to the social contract. The constraint's equality protections do not reach them; the Constitution explicitly protects slavery (Three-Fifths Clause, Fugitive Slave Clause, 1808 importation barrier). Exit is impossible within the system; resistance meets overwhelming structural suppression. The 13th/14th/15th Amendments formally altered this but the restrictive reading treats those as narrow exceptions, not universal principles.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_people, payer,
    powerless, generational, trapped, national).

% White men without property qualifications face barriers to suffrage and office-holding in the early republic. Some gains through Jacksonian democracy (property qualifications dropped), but the restrictive reading treats their inclusion as contingent legislative grace, not constitutional right. Exit is constrained — they share race/gender with beneficiaries but lack the property status that defines the sovereign class.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_propertied_white_males, payer,
    moderate, biographical, constrained, national).

% Excluded from the social contract entirely — treated as foreign nations or domestic dependent nations, not constitutional persons. The constraint's equality protections do not apply; their sovereignty is overridden by plenary power doctrine. Exit is structurally impossible — they are subjected to U.S. constitutional authority without its protections. The restrictive reading naturalizes this exclusion as the founding design.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, indigenous_peoples, payer,
    powerless, generational, trapped, national).

% Administer and enforce the restrictive reading through judicial review. They set the interpretive agenda: original public meaning, fixed at founding, narrow scope. Their institutional legitimacy depends on maintaining this methodology. They benefit professionally and ideologically from the reading's dominance — it validates their interpretive authority. The directionality override (d=0.25) reflects partial capture: they appear as neutral coordinators but their methodology systematically favors the narrow beneficiary set.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_judges, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, originalist_judges, beneficiary).

% Their academic and professional standing is built on the restrictive originalist methodology. The reading validates their interpretive framework, giving them institutional positions, judicial influence, and intellectual authority. Their identity is fused with the methodology — exit means abandoning their professional self-concept. They benefit from the constraint's persistence without directly enforcing it.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_scholars, beneficiary,
    organized, biographical, identity_locked, national).

% Advocate for equality as universal human right applying to all persons regardless of historical exclusions. They are structurally excluded from the restrictive reading's framework — their position is treated as judicial activism, not interpretation. They would object to the narrow beneficiary set but cannot register that objection within the originalist framework; their only path is constitutional amendment or Court composition change, both blocked by the high legitimacy threshold.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansive_universalist_advocates, excluded,
    organized, generational, trapped, national).

% Argue the constitutional text contains an equality principle but its application expands through democratic amendment (Article V), not judicial reinterpretation. They accept the restrictive reading's premise of fixed textual meaning but contest its scope — they would include women and minorities through formal amendment. They are excluded from the restrictive reading's interpretive community but have a structural path (amendment) that the restrictive reading makes extremely difficult.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, progressive_textualist_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the full structure from outside the contest. They see the kernel (equality clause), the three readings, and the historical trajectory of inclusion/exclusion. They neither collect from nor pay into the constraint — their role is to map the structural relationships and measure the drift between founding design and contemporary operation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates stable political equality among propertied white males as the sovereign political actors of the 18th-century social contract — defines who counts as a party to the constitutional compact and coordinates their mutual rights and obligations.
% TRANSFER_FUNCTION: Moves political rights, legal personhood, suffrage, office-holding, and sovereign authority from the excluded (women, enslaved people, non-propertied white males, indigenous peoples) to propertied white males, using the Constitution's text and structure as the transfer mechanism.
% ABSENT_VOICES: Women, enslaved people, indigenous peoples, and non-propertied men were not in the room at the founding — they could not consent to the social contract that defined them out of equality. Their objections were structurally impossible to register in 1787; subsequent generations face the Article V amendment threshold designed to require the beneficiaries' consent for any expansion.
% DISAPPEARANCE_RATIONALE: If the restrictive originalist reading vanished overnight, constitutional law would reorganize around broader equality principles — the 13th/14th/15th/19th/24th/26th Amendments would become the operative baseline rather than exceptions to a narrow founding design. Citizenship, suffrage, and rights doctrines would reconstruct from universal premises. The entire edifice of originalist jurisprudence would lose its anchoring constraint.
% FOUNDING_PROBLEM: How to constitute a stable republican government among propertied white male citizens while preserving slavery, patriarchal property relations, and indigenous displacement — the 1787 Constitution solved this by defining the sovereign political actors narrowly and embedding their dominance in structural mechanisms (Senate, Electoral College, Three-Fifths Clause, Article V).
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era (Gordon Wood, Bernard Bailyn, Edmund Morgan) attest the Constitution was designed for propertied white male sovereignty with slavery protected. Critical race theorists (Derrick Bell, Kimberlé Crenshaw) and feminist legal scholars (Catharine MacKinnon) corroborate the exclusionary design from outside the beneficiary set. The founding problem (constituting a slaveholding republic) is gone; the interpretive legacy persists.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78 at founding) is high because the constraint moves political rights, legal personhood, and sovereign authority from the excluded majority to a narrow propertied white male minority. Suppression (0.85) is very high because the constraint's persistence depends on actively excluding rival readings through judicial doctrine, constitutional structure, and the amendment threshold — not on participant preference. Theater ratio (0.42 at present) reflects that the originalist methodology performs faithful interpretation while the substantive outcome maintains exclusion; the gap has grown as formal barriers fell but interpretive resistance remains. Accessibility collapse (0.78) is high because the reading's internal logic (fixed meaning at founding) structurally closes off expansive interpretations within its own framework. Resistance (0.82) is high because the excluded groups and their allies have continuously contested this reading through social movements, litigation, and constitutional amendment.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (propertied white males) and the agenda_setter seat (originalist judges) should compute as rope/mountain from their positions: the constraint appears as stable coordination among the sovereign political actors. The victim seats (women, enslaved, non-propertied, indigenous) compute as snare: the same structure operates as enforced extraction and denial of personhood. The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the hybrid nature: genuine coordination among the included, asymmetric extraction from the excluded.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white males are structural beneficiaries (d near 0.0) — they collect the full equality protections, political rights, and sovereign authority. Women, enslaved people, non-propertied white males, and indigenous peoples are structural targets (d near 1.0) — they bear the costs of exclusion, denied personhood, and political subordination. Originalist judges are agenda_setters (d ~0.3) — they administer the constraint but their institutional role gives them some insulation from direct extraction. Originalist scholars are beneficiaries (d ~0.15) — their interpretive framework gains legitimacy and institutional purchase from this reading. Expansive universalist and progressive textualist advocates are excluded (d ~0.7) — they would challenge the constraint but are kept out by the high legitimacy threshold. Constitutional scholars are observers (d = 0.5) — analytical seat sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — constituting a stable republican government among propertied white males while preserving slavery and patriarchal property relations — is dead (founding_problem_status: dead). The arrangement persists because the interpretive methodology (originalism) has become self-validating: it treats the founding exclusions as fixed meaning rather than historical contingency. This is mandatrophy — the constraint's mandate (founding-era social contract) has outlived its function, but the interpretive machinery maintains it. The high legitimacy threshold for expansion (requiring formal amendment) prevents democratic correction, converting what was a coordination mechanism for 1787 actors into a snare for 2025 subjects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (equality_clause_scope) rather than a standalone constraint?',
    'Committer frame: the kernel_id and reading_id are declared in the generation manifest; this omega records that identity for downstream analysis.',
    'If treated as standalone, the constraint''s ε would be mis-specified — the reading''s ε is indexed to the standing arrangement under contest (the exclusionary constitutional order), not to the reading''s endorsed alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee frame identity: this is restrictive_originalist reading of equality_clause_scope kernel').

omega_variable(
    sibling_reading_structural_delta,
    'What would change structurally if a sibling reading (expansive_universalist or progressive_textualist) were instantiated instead?',
    'Compare beneficiary/victim sets, enforcement requirements, and legitimacy thresholds across the three readings.',
    'Expansive_universalist eliminates the victim set entirely (universal beneficiaries); progressive_textualist shifts expansion to democratic amendment process, altering the agenda_setter role from courts to legislatures. Both would collapse the current extraction structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural delta between this reading and its siblings: beneficiary set, victim set, enforcement locus, legitimacy threshold').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.85) primarily structural (legal barriers, judicial doctrine) or internalized (excluded groups accepting exclusion as natural)?',
    'Historical analysis of resistance movements: if suppression persists after legal barriers fall (e.g., post-13th/14th/15th/19th Amendments), internalized component is significant.',
    'If substantially internalized, effective suppression exceeds structural measure — the constraint''s extraction continues through cognitive channels even after formal barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the exclusionary constitutional order').

omega_variable(
    originalist_capture_risk,
    'Are originalist judges/scholars genuine agenda_setters administering a coordination function, or are they partly captured by the narrow beneficiary set (propertied white males)?',
    'Track doctrinal outcomes: when originalist methodology produces results that harm the narrow beneficiary set, does adherence hold? If not, capture is indicated.',
    'If captured, the agenda_setter role masks beneficiary status — directionality for originalist_judges should be overridden toward beneficiary end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalist_capture_risk, conceptual, 'Whether the interpretive elite are coordinators or captured beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equality_restrictive_originalist_tr_t1787, equality_clause_scope__restrictive_originalist, theater_ratio, 1787, 0.15).
narrative_ontology:measurement(equality_restrictive_originalist_tr_t1865, equality_clause_scope__restrictive_originalist, theater_ratio, 1865, 0.25).
narrative_ontology:measurement(equality_restrictive_originalist_tr_t1870, equality_clause_scope__restrictive_originalist, theater_ratio, 1870, 0.3).
narrative_ontology:measurement(equality_restrictive_originalist_tr_t1920, equality_clause_scope__restrictive_originalist, theater_ratio, 1920, 0.38).
narrative_ontology:measurement(equality_restrictive_originalist_tr_t1954, equality_clause_scope__restrictive_originalist, theater_ratio, 1954, 0.42).
narrative_ontology:measurement(equality_restrictive_originalist_tr_t1965, equality_clause_scope__restrictive_originalist, theater_ratio, 1965, 0.4).
narrative_ontology:measurement(equality_restrictive_originalist_tr_t2025, equality_clause_scope__restrictive_originalist, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(equality_restrictive_originalist_be_t1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 1787, 0.85).
narrative_ontology:measurement(equality_restrictive_originalist_be_t1865, equality_clause_scope__restrictive_originalist, base_extractiveness, 1865, 0.72).
narrative_ontology:measurement(equality_restrictive_originalist_be_t1870, equality_clause_scope__restrictive_originalist, base_extractiveness, 1870, 0.68).
narrative_ontology:measurement(equality_restrictive_originalist_be_t1920, equality_clause_scope__restrictive_originalist, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(equality_restrictive_originalist_be_t1954, equality_clause_scope__restrictive_originalist, base_extractiveness, 1954, 0.48).
narrative_ontology:measurement(equality_restrictive_originalist_be_t1965, equality_clause_scope__restrictive_originalist, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(equality_restrictive_originalist_be_t2025, equality_clause_scope__restrictive_originalist, base_extractiveness, 2025, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(equality_restrictive_originalist_su_t1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 1787, 0.9).
narrative_ontology:measurement(equality_restrictive_originalist_su_t1865, equality_clause_scope__restrictive_originalist, suppression_requirement, 1865, 0.75).
narrative_ontology:measurement(equality_restrictive_originalist_su_t1870, equality_clause_scope__restrictive_originalist, suppression_requirement, 1870, 0.7).
narrative_ontology:measurement(equality_restrictive_originalist_su_t1920, equality_clause_scope__restrictive_originalist, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(equality_restrictive_originalist_su_t1954, equality_clause_scope__restrictive_originalist, suppression_requirement, 1954, 0.5).
narrative_ontology:measurement(equality_restrictive_originalist_su_t1965, equality_clause_scope__restrictive_originalist, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(equality_restrictive_originalist_su_t2025, equality_clause_scope__restrictive_originalist, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__restrictive_originalist, 0.08).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% BGS-pattern decomposition of 'equality clause scope' kernel into three structurally distinct readings with different ε values, beneficiary/victim sets, and enforcement loci. Restrictive originalist has high extraction (0.78) and high suppression (0.85); expansive universalist has near-zero extraction; progressive textualist has moderate extraction with democratic enforcement locus. All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__restrictive_originalist, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
