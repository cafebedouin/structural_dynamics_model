% ============================================================================
% CONSTRAINT STORY: permissive_license_text__commons_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__commons_coordination_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: permissive_license_text__commons_coordination_reading
 *   human_readable: Permissive Open Source License as Commons Coordination
 *   domain: software/intellectual_property/governance
 *
 * SUMMARY:
 *   This constraint describes permissive open-source licensing (MIT, BSD,
 *   Apache 2.0) as a coordination mechanism that minimizes legal friction for
 *   implementers, maximizing the pool of contributors and derivative
 *   creators. This is ONE READING of the contested kernel
 *   'permissive_license_text'. The reading instantiates the perspective that
 *   legal relaxation (no reciprocity requirement) enables abundance and
 *   universal participation. This is structurally distinct from the
 *   copyleft_counterfactual_reading (which claims permissive licensing
 *   enables uncompensated extraction and advocates copyleft as the necessary
 *   alternative) and from the corporate_moat_reading (which claims permissive
 *   licensing is a strategy to build market moats on uncompensated
 *   derivatives). Each reading produces a different epsilon, different
 *   beneficiary/victim structure, and different classification. This story
 *   treats permissive licensing as genuine coordination with low
 *   extractiveness and no victim set — the authored metrics reflect this
 *   reading's epistemic stance.
 *
 * KEY AGENTS:
 *   - original_author: Creator of the permissive-licensed work; benefits from attribution and ecosystem adoption without enforcement burden.
 *   - universal_implementer_pool: Any organization or individual; benefits from low legal friction and no reciprocity obligation.
 *   - downstream_derivative_creators: Commercial and open-source vendors building on permissive code; benefit from arbitrage opportunity and license compatibility.
 *   - end_users: Benefit indirectly from software abundance and competitive innovation.
 *   - legal_framework_implementers: Courts and legislatures recognizing permissive licensing as valid; enable the constraint's persistence.
 *   - excluded: proprietary_authors and reciprocal_license_advocates — their objections are structurally outside this reading's frame and routed to omegas.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__commons_coordination_reading, 0.18).
domain_priors:suppression_score(permissive_license_text__commons_coordination_reading, 0.05).
domain_priors:theater_ratio(permissive_license_text__commons_coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__commons_coordination_reading, rope).
narrative_ontology:human_readable(permissive_license_text__commons_coordination_reading, "Permissive Open Source License as Commons Coordination").
narrative_ontology:topic_domain(permissive_license_text__commons_coordination_reading, "software/intellectual_property/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__commons_coordination_reading, 'b1db879d-467d-4ad4-a759-df505b5238d7').
narrative_ontology:cs_kernel_codification('b1db879d-467d-4ad4-a759-df505b5238d7', fixed_text).
narrative_ontology:cs_authority_grounding('b1db879d-467d-4ad4-a759-df505b5238d7', practice).
narrative_ontology:cs_interpretation_layer_present('b1db879d-467d-4ad4-a759-df505b5238d7').
narrative_ontology:cs_reading_relation('b1db879d-467d-4ad4-a759-df505b5238d7', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1db879d-467d-4ad4-a759-df505b5238d7', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('b1db879d-467d-4ad4-a759-df505b5238d7', foundational, permissive_license_enables_abundance).
narrative_ontology:cs_axiom_status(permissive_license_enables_abundance, holdable).
narrative_ontology:cs_axiom_grounding('b1db879d-467d-4ad4-a759-df505b5238d7', permissive_license_enables_abundance, empirically_contingent).
narrative_ontology:cs_axiom('b1db879d-467d-4ad4-a759-df505b5238d7', foundational, voluntary_author_choice_legitimates_uncompensated_derivative).
narrative_ontology:cs_axiom_status(voluntary_author_choice_legitimates_uncompensated_derivative, holdable).
narrative_ontology:cs_axiom_grounding('b1db879d-467d-4ad4-a759-df505b5238d7', voluntary_author_choice_legitimates_uncompensated_derivative, deontological).
narrative_ontology:cs_reference_frame('b1db879d-467d-4ad4-a759-df505b5238d7', permissive_license_voluntary_author_choice).
narrative_ontology:cs_drift_state('b1db879d-467d-4ad4-a759-df505b5238d7', contemporary_vendor_scale, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b1db879d-467d-4ad4-a759-df505b5238d7', '').
narrative_ontology:cs_kernel_id(permissive_license_text__commons_coordination_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, universal_implementer_pool).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, downstream_derivative_creators).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, end_users_through_abundance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, original_author).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains attribution and moral ownership of the work; benefits from community contributions, fork visibility, and reputation as an open-source steward. The permissive license removes the burden of enforcing reciprocity while preserving credit assignment. Can fork off or dual-license if the community diverges.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, original_author, beneficiary,
    moderate, generational, mobile, global).

% Any individual or organization can use, modify, and redistribute the software with minimal legal friction. No requirement to contribute back or disclose modifications. The implementation pool grows because entry friction is low; participants benefit from the commons without debt.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, universal_implementer_pool, beneficiary,
    organized, biographical, mobile, global).

% Can build proprietary products on top of permissive-licensed code without obligation to share improvements or release their derivative under the same license. This freedom attracts commercial investment and accelerates product cycles.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, downstream_derivative_creators, beneficiary,
    powerful, biographical, arbitrage, global).

% Benefit indirectly from ecosystem diversity and abundance: the low barrier to derivative creation means more implementations, more competition, faster innovation. Software abundance flows from the low legal friction.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, end_users, beneficiary,
    powerless, immediate, constrained, global).

% Prefer copyleft (GPL) or closed-source models to retain exclusive control over derivatives. Permissive licensing is outside their frame — they advocate for stricter IP enforcement, not for participation in the permissive commons. Their position is not represented in this reading's stakeholder set.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, proprietary_authors, excluded,
    powerful, biographical, arbitrage, global).

% Argue that permissive licensing enables free-riding and uncompensated extraction by proprietary vendors. They advocate for GPL-style copyleft as the only ethically coherent open-source model. Under this reading, their objection is routed to an omega.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, reciprocal_license_advocates, excluded,
    organized, generational, mobile, global).

% Legislatures, courts, and standard-setting bodies that define copyright scope and license enforceability. They enable permissive licensing by recognizing it as a valid exercise of copyright holders' rights (not forcing reciprocity). The constraint's persistence depends on their continued recognition.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, legal_framework_implementers, agenda_setter,
    institutional, generational, analytical, national).

% The distributed ecosystem of shared, reusable components that permissive licensing enables. Not an actor but a coordination outcome: the aggregate value created by low-friction participation.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, open_source_ecosystem, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(permissive_license_text__commons_coordination_reading, open_source_ecosystem).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__commons_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(permissive_license_text__commons_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the commons-contribution problem: original authors offer code under a license that removes legal friction for downstream use, modification, and redistribution. This lowers the cost of collaboration and reuse, enabling a larger pool of implementers to build on shared foundations without negotiating individual permissions or reciprocity terms.
% TRANSFER_FUNCTION: Transfers the legal right to use, modify, and redistribute code from author to implementer without requiring return of improvements or derivative disclosure. The implementer receives implementation freedom; the author receives attribution and the ecosystem benefit of broader adoption.
% ABSENT_VOICES: Proprietary vendors who benefit from the permissive code but advocate for stricter IP enforcement for their own derivatives are excluded from this reading's stakeholder set. Copyleft advocates who argue that permissive licensing enables uncompensated extraction and advocate GPL reciprocity are also structurally excluded — their position is a sibling reading's standpoint, not a dissenting voice within this frame.
% DISAPPEARANCE_RATIONALE: If permissive licensing and its legal framework disappeared, the software commons would reorganize around either copyleft (GPL) reciprocity requirements, closed-source models, or patent/negotiation pools. The universal implementer pool would fragment into proprietary or copyleft subcommunities. Reuse and derivative creation would slow as legal friction returned. The empirical abundance enabled by low friction would diminish.
% FOUNDING_PROBLEM: Early software development was fragmented by license incompatibility and permission-seeking overhead. Authors who wanted to share code faced the choice: enforce strict reciprocity (copyleft), close everything (proprietary), or invest in per-use licensing. Permissive licensing emerged to solve the friction problem for authors who valued ecosystem participation over controlling derivatives.
% FOUNDING_PROBLEM_CORROBORATION: Original authors of major permissive-licensed projects (Linux kernel maintainers choosing MIT/GPL compatibility, Django/Flask creators on BSD simplicity reducing friction, React maintainers on permissive-first ecosystem diversity) and open-source foundations (Apache, Eclipse, Linux Foundation) document that low legal friction enables broader participation and ecosystem growth. Empirical data on GitHub fork velocity and npm package reuse consistently show higher reuse rates under permissive vs. copyleft licensing, attributed by community analysis to friction reduction.
narrative_ontology:disappearance_verdict(permissive_license_text__commons_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__commons_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__commons_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__commons_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__commons_coordination_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__commons_coordination_reading_tests).
:- end_tests(permissive_license_text__commons_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) under this reading because the arrangement solves a genuine coordination problem (friction reduction) and the author retains moral and reputational ownership without enforcing control. The measurement series shows extractiveness slowly rising from 0.12 to 0.18 over the interval, reflecting an empirical dynamic: as proprietary vendors increasingly rely on permissive code without contributing back, the effective extraction (in the sense of uncompensated upstream benefit) grows — but this reading interprets that dynamic as a side effect of success, not as the constraint's true function. Suppression is negligible (0.05) because participation is voluntary and exit costs are low; the constraint depends on authors' choice to license permissively, not on coercion. Theater ratio is near-minimal (0.08) because the coordination function is real: license text directly enables reuse and derivative creation; the performative component is minimal. Accessibility collapse is low (0.15) because exit alternatives (copyleft, proprietary licensing, no sharing) remain available to authors; the permissive path is attractive but not forced. Resistance is low (0.12) because the constraint benefits a broad implementer pool; opposition is concentrated among proprietary vendors and copyleft advocates, not distributed across many powerless agents.
 *
 * PERSPECTIVAL GAP:
 *   Original authors under this reading see permissive licensing as a gift of control yielding community benefit and reputation. The corporate_moat reading sees the same authors as extractively vulnerable — their code becomes raw material for proprietary vendors' profit. The universal implementer pool under this reading sees low-friction access to shared infrastructure. The copyleft_counterfactual reading sees them as free-riders who ought to contribute back. The constraint's persistent form does not change; the reading changes what extraction means and who bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Original authors derive d near 0.3–0.4: they are mild beneficiaries (reputation, adoption, ecosystem credit) with some control surrendered (no reciprocity obligation enforcement). The universal implementer pool derives d near 0.05–0.15: they are strong beneficiaries (low legal friction, implementation freedom, arbitrage access) with minimal costs or identity risk. Downstream derivative creators derive d near 0.1–0.2: they benefit substantially (can build proprietary products without obligation) but have no structural vulnerability to the constraint (they could choose copyleft or closed-source if it served their interests better). End users derive d near 0.05: they benefit from indirect abundance with zero direct cost. Proprietary authors and copyleft advocates are EXCLUDED: their directional relationship to this constraint is not computed because they oppose the reading's core premise and are not stakeholders within the commons_coordination_reading frame — they are stakeholders in sibling readings (corporate_moat_reading and copyleft_counterfactual_reading).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (license friction impeding participation) remains live: original authors continue to face the choice of reciprocity enforcement overhead vs. permissive openness. The constraint has not undergone mandatrophy because the problem it solves has not atrophied — the empirical data on fork velocity and reuse rates (measured via GitHub dependency analysis, npm package managers) consistently show permissive licensing enabling broader participation than copyleft. The constraint may be approaching what the sibling readings claim is mandatrophy (the original coordination problem is solved; now the constraint is just a vehicle for uncompensated extraction), but under this reading that claim is rejected as a mischaracterization of the founding problem: the founding problem is not 'ensure reciprocity' but 'enable participation', and permissive licensing continues to solve the latter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uncompensated_derivative_extraction,
    'Does the measurable economic value flowing to proprietary vendors building on permissive code, without reciprocal contribution, constitute an uncompensated extraction from the original author''s perspective?',
    'Longitudinal study of original authors'' responses: Do they express satisfaction with ecosystem outcomes (abundance, attribution, adoption) or regret at uncompensated vendor capture? Post-hoc license switching (e.g., authors converting permissive projects to copyleft) is direct signal.',
    'If extraction is perceived as high by authors, the corporate_moat reading becomes the dominant framing and this reading reclassifies upward. If authors consistently express satisfaction, the commons_coordination reading holds and extraction remains low.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(uncompensated_derivative_extraction, empirical, 'Whether author satisfaction with permissive licensing persists or shifts toward copyleft adoption.').

omega_variable(
    free_riding_vs_voluntary_contribution,
    'Is the non-reciprocal participation of proprietary vendors a form of free-riding (as the copyleft_counterfactual reading claims) or a legitimate exercise of rights the author deliberately granted (as this reading claims)?',
    'Normative analysis of license intent: Do author interviews, license-adoption rationales, and community norms support the framing ''this is the arrangement we chose'' (commons reading) or ''this is the loophole we now regret'' (copyleft reading)?',
    'The frame selects which reading is ''correct''. No empirical fact resolves it; the answer depends on accepting the author''s stated intent (commons) or rejecting it as cover story (copyleft/corporate_moat). This omega documents the irreducible interpretive disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(free_riding_vs_voluntary_contribution, conceptual, 'Whether non-reciprocal participation is free-riding or legitimate under the voluntary grant.').

omega_variable(
    ecosystem_abundance_causation,
    'Does the empirically observed growth in open-source projects, ecosystem reuse rates, and derivative creation rate actually causally depend on permissive licensing, or would copyleft licensing (or other mechanisms) produce similar abundance?',
    'Natural experiments from jurisdictions or communities with copyleft-only mandates; comparison of ecosystem density and innovation rates; counterfactual modeling of adoption curves under GPL-only scenarios.',
    'If permissive licensing is the causal driver of abundance, the commons_coordination reading is robust. If similar abundance would result under copyleft or closed-source models, the constraint is neither causally necessary nor specifically coordination — it is merely one coordination mechanism among peers, and the corporate_moat reading gains force (the extractiveness of the vendor moat becomes the salient feature).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_abundance_causation, empirical, 'Whether ecosystem abundance causally depends on permissive licensing or would occur under alternative regimes.').

omega_variable(
    kernel_reading_underdetermination,
    'The kernel (permissive-license text) is ambiguous about what problem it solves: friction reduction (this reading) vs. market moat enablement (corporate_moat_reading) vs. missing-reciprocity problem (copyleft_counterfactual_reading). Which reading is the ''correct'' interpretation of the license text?',
    'None. The kernel text does not specify which reading is correct. Different authors have written permissive licenses for different reasons (Stallman''s concerns about compatibility vs. Linus''s indifference to derivatives vs. Apache''s desire for corporate participation). No fact about the license text itself resolves the ambiguity.',
    'The three readings are structurally incommensurable: they disagree on what the constraint IS, not on what happens if it operates. This omega documents that the reading contest is not resolvable by evidence alone — the reading itself is a choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'The kernel-reading underdetermination: permissive text supports multiple incompatible readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__commons_coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(permissive_license_commons_tr_t0, permissive_license_text__commons_coordination_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(permissive_license_commons_tr_t5, permissive_license_text__commons_coordination_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(permissive_license_commons_tr_t10, permissive_license_text__commons_coordination_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(permissive_license_commons_tr_t15, permissive_license_text__commons_coordination_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(permissive_license_commons_tr_t20, permissive_license_text__commons_coordination_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(permissive_license_commons_tr_t25, permissive_license_text__commons_coordination_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(permissive_license_commons_tr_t30, permissive_license_text__commons_coordination_reading, theater_ratio, 30, 0.08).

% Extraction over time
narrative_ontology:measurement(permissive_license_commons_be_t0, permissive_license_text__commons_coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(permissive_license_commons_be_t5, permissive_license_text__commons_coordination_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(permissive_license_commons_be_t10, permissive_license_text__commons_coordination_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(permissive_license_commons_be_t15, permissive_license_text__commons_coordination_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(permissive_license_commons_be_t20, permissive_license_text__commons_coordination_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(permissive_license_commons_be_t25, permissive_license_text__commons_coordination_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(permissive_license_commons_be_t30, permissive_license_text__commons_coordination_reading, base_extractiveness, 30, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(permissive_license_commons_su_t0, permissive_license_text__commons_coordination_reading, suppression_requirement, 0, 0.04).
narrative_ontology:measurement(permissive_license_commons_su_t5, permissive_license_text__commons_coordination_reading, suppression_requirement, 5, 0.045).
narrative_ontology:measurement(permissive_license_commons_su_t10, permissive_license_text__commons_coordination_reading, suppression_requirement, 10, 0.047).
narrative_ontology:measurement(permissive_license_commons_su_t15, permissive_license_text__commons_coordination_reading, suppression_requirement, 15, 0.048).
narrative_ontology:measurement(permissive_license_commons_su_t20, permissive_license_text__commons_coordination_reading, suppression_requirement, 20, 0.049).
narrative_ontology:measurement(permissive_license_commons_su_t25, permissive_license_text__commons_coordination_reading, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(permissive_license_commons_su_t30, permissive_license_text__commons_coordination_reading, suppression_requirement, 30, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__commons_coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(permissive_license_text__commons_coordination_reading, 0.05).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% The kernel 'permissive_license_text' generates three constraint stories, each representing a different reading: (1) commons_coordination_reading: low epsilon, universal implementer beneficiaries, no victims — interprets permissive licensing as friction reduction enabling broad participation. (2) corporate_moat_reading: higher epsilon, identifies proprietary vendors as extractors and original authors as implicit victims — interprets permissive licensing as a moat-building strategy. (3) copyleft_counterfactual_reading: examines the counterfactual GPL alternative, arguing reciprocity produces better outcomes for original authors and communities. Each story has distinct epsilon, beneficiary/victim structure, and founding_problem interpretation. The readings do not compete on facts; they compete on what the license text means. All three stories link via network.affects_constraints to register the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
