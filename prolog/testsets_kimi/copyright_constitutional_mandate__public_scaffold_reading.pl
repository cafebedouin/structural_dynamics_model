% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright Constitutional Mandate â Public Scaffold Reading
 *   domain: legal/constitutional/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the public-scaffold reading of the
 *   Copyright Clause kernel: the constitutional mandate that copyright secure
 *   exclusive rights only for 'limited Times' in order to 'promote the
 *   Progress of Science and useful Arts.' In this reading, the temporary
 *   monopoly is instrumental scaffolding erected to solve a public-goods
 *   problem, with the public domain as the intended beneficiary and the
 *   sunset into commons as the structural telos. The reading competes with a
 *   corporate-enclosure reading (maximal property protection) and a
 *   judicial-ambiguity reading (uncabined legislative discretion). The
 *   authored metrics track the drift of this scaffold toward theatrical
 *   maintenance as statutory terms have repeatedly extended.
 *
 * KEY AGENTS:
 *   - general_public (beneficiary/generational) â ultimately receives the public domain enrichment
 *   - follow_on_creators (beneficiary/biographical) â builds on expired works without licensing
 *   - contemporary_users (payer/biographical) â bears monopoly pricing during the temporary term
 *   - content_holders (dual payer-beneficiary/powerful) â granted temporary exclusive rights but constrained by expiration requirement
 *   - congress (agenda_setter/institutional) â sets terms under constitutional mandate
 *   - public_domain_advocates (observer/moderate) â monitors drift and advocates for shorter terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.55).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.4).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright Constitutional Mandate â Public Scaffold Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "legal/constitutional/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '4292a3f6-cacc-4f5c-8952-ecb877c94517').
narrative_ontology:cs_kernel_codification('4292a3f6-cacc-4f5c-8952-ecb877c94517', fixed_text).
narrative_ontology:cs_authority_grounding('4292a3f6-cacc-4f5c-8952-ecb877c94517', lineage).
narrative_ontology:cs_interpretation_layer_present('4292a3f6-cacc-4f5c-8952-ecb877c94517').
narrative_ontology:cs_reading_relation('4292a3f6-cacc-4f5c-8952-ecb877c94517', copyright_constitutional_mandate__corporate_enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('4292a3f6-cacc-4f5c-8952-ecb877c94517', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('4292a3f6-cacc-4f5c-8952-ecb877c94517', foundational, temporary_monopoly_for_public_end).
narrative_ontology:cs_axiom_status(temporary_monopoly_for_public_end, holdable).
narrative_ontology:cs_axiom_grounding('4292a3f6-cacc-4f5c-8952-ecb877c94517', temporary_monopoly_for_public_end, conventional).
narrative_ontology:cs_axiom('4292a3f6-cacc-4f5c-8952-ecb877c94517', foundational, public_domain_as_constitutional_telos).
narrative_ontology:cs_axiom_status(public_domain_as_constitutional_telos, holdable).
narrative_ontology:cs_axiom_grounding('4292a3f6-cacc-4f5c-8952-ecb877c94517', public_domain_as_constitutional_telos, conventional).
narrative_ontology:cs_reference_frame('4292a3f6-cacc-4f5c-8952-ecb877c94517', limited_monopoly_public_enrichment_framework).
narrative_ontology:cs_drift_state('4292a3f6-cacc-4f5c-8952-ecb877c94517', post_sonny_bono_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4292a3f6-cacc-4f5c-8952-ecb877c94517', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, general_public).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, follow_on_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, content_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__public_scaffold_reading, contemporary_users).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__public_scaffold_reading, content_holders).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, limited_times_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, public_domain_enrichment_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the ultimate benefit of the copyright system as creative works enter the public domain after limited terms. During the temporary monopoly period, contemporary members forgo free access and pay elevated prices or licensing fees for in-copyright works as the coordination cost of eventual commons enrichment.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, general_public, beneficiary,
    organized, generational, mobile, national).

% Gains freedom to build upon, adapt, transform, and disseminate expired works without permission or licensing barriers. Benefits from a growing public domain, though repeated term extensions delay that benefit and expand the zone of required clearance.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, follow_on_creators, beneficiary,
    moderate, biographical, mobile, national).

% Pays monopoly prices, licensing fees, or access charges to use in-copyright works during the limited term. Free copying alternatives exist technically but are legally suppressed, making lawful access a purchased good contingent on the temporary scaffold.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, contemporary_users, payer,
    organized, biographical, constrained, national).

% Granted temporary exclusive rights as the incentive mechanism for creation and dissemination. Constrained by the constitutional requirement that monopoly expire after limited times, which caps the duration of rent extraction and ultimately transfers the work to the public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, content_holders, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, content_holders, beneficiary).

% Sets statutory copyright terms, exceptions, and enforcement provisions. Structurally authorized by the Progress Clause to secure exclusive rights only for limited times in service of public enrichment, though subject to persistent lobbying for term expansion.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% Monitors copyright term expansion, retroactive extensions, and enclosure trends. Argues for shorter terms and broader fair use to preserve the scaffold function, providing external analytical pressure on the agenda setter.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain_advocates, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the public goods problem of creative production and dissemination â information is non-rival and non-excludable, so private markets under-produce it without an exclusive-rights incentive â by granting temporary monopolies that are designed to expire into the public domain.
% TRANSFER_FUNCTION: Moves creative works from temporary private exclusivity into the public domain after limited times. During the term, moves monopoly rents from contemporary users to content holders as the incentive payment, with the understanding that this transfer is temporary and instrumental.
% ABSENT_VOICES: Perpetual copyright advocates and content-industry maximalists are present in legislative lobbying but their preferred reading of the kernel as permanent property is structurally excluded by the Progress Clause's limited-times requirement. The contemporary public is diffuse and politically underorganized relative to concentrated content industries.
% DISAPPEARANCE_RATIONALE: If the constitutional mandate for limited times and public-purpose grounding vanished, Congress could grant perpetual or near-perpetual copyrights; the public domain would cease to grow, follow-on creators would face universal licensing requirements for existing works, and the information ecology would reorganize around permanent enclosure rather than temporary scaffolding.
% FOUNDING_PROBLEM: Creative works are public goods: they are non-rival, non-excludable, and vulnerable to free-riding, resulting in systematic under-production and under-dissemination without some mechanism to incentivize creation and distribution.
% FOUNDING_PROBLEM_CORROBORATION: Economists and legal scholars outside the content industry attest to the public-goods character of information and the undersupply problem. Public domain advocates and tech-sector observers corroborate that the problem persists but argue that alternative incentive models â patronage, crowdfunding, open access, and subsidy â reduce the necessary monopoly duration and challenge the scaffold's current scale.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the monopoly regime does extract from contemporary users during the term, even if justified as transitional. Suppression is moderate (0.40) because unauthorized copying is legally suppressed but not through heavy criminal enforcement for non-commercial use. Theater ratio is moderate (0.35) and rising over the measurement interval because the 'limited times' requirement has become increasingly performative as retroactive extensions approach effective perpetuity; the sunset clause is still present in form but diluted in function. Accessibility collapse is moderate (0.50) because legal alternatives to licensing collapse once the copyright framework is accepted, though technical workarounds remain widely available. Resistance is moderate (0.45) reflecting sustained pushback from copyleft movements, digital-rights advocates, and file-sharing networks.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Congress) experiences the constraint as a functioning coordination mechanism that incentivizes creative industries. Content holders experience it as an insufficiently generous incentive that expires too soon. Contemporary users experience the same structure as extraction during the term. Future public and follow-on creators experience it as delayed subsidy. The engine computes these divergences from the structural data rather than resolving them.
 *
 * DIRECTIONALITY LOGIC:
 *   General_public and follow_on_creators are declared beneficiaries (low d), reflecting the reading's structural claim that the public domain is the ultimate beneficiary of the regime. Contemporary_users are payers (high d) because they bear the temporary monopoly cost. Content_holders are structurally mixed: they receive the temporary monopoly rent but are constrained by the expiration requirement; because they are not declared in base_properties.beneficiaries, their directionality derives from their exit and power profile, placing them nearer the target end than a pure beneficiary. Congress, as agenda_setter with analytical exit, sits at a neutral-agenda distance.
 *
 * MANDATROPHY ANALYSIS:
 *   This scaffold reading prevents mislabeling the constraint as pure extraction (snare) or permanent property (corporate enclosure) by insisting on the sunset function. However, the temporal measurements show base_extractiveness and theater_ratio rising together over 230 years, indicating that the mandate may have outlived its transitional justification â the T17 abductive trigger would flag mountain_extraction_accumulation if this were claimed as a mountain. Because it is claimed as scaffold, the drift signals a possible piton transition: the sunset clause is still present but increasingly theatrical. The founding_problem_status remains live while the disappearance_verdict is world_rearranges, so mandatrophy is not yet resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the public-scaffold reading or the corporate-enclosure reading the operative structure of contemporary copyright law?',
    'Comparative analysis of legislative history, judicial doctrine (including Eldred v. Ashcroft and its dissent), and statutory term lengths against the constitutional text.',
    'If corporate enclosure is the operative structure, this constraint is a false scaffold (potentially piton or snare) rather than a genuine transitional coordination mechanism, and the beneficiaries should be reclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the copyright kernel is structurally operative in practice.').

omega_variable(
    limited_times_erosion,
    'Do repeated retroactive term extensions and ever-longer initial terms convert the ''limited times'' requirement into a de facto perpetual monopoly?',
    'Empirical measurement of effective copyright duration versus creative industry investment responsiveness; constitutional challenge evidence and amicus briefing.',
    'If terms are effectively unlimited, the sunset clause is theatrical and the scaffold has degraded toward a piton or snare, invalidating the low-victim coordination framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(limited_times_erosion, empirical, 'Whether limited times has eroded into effective perpetuity.').

omega_variable(
    public_goods_alternatives,
    'Have alternative incentive models reduced the necessity of copyright monopoly as a scaffold for creative production?',
    'Cross-national and cross-sectoral comparison of creative output under varying copyright regimes, open-access models, patronage platforms, and subsidy systems.',
    'If alternatives suffice, the coordination function of the scaffold is weaker than claimed and the extraction from contemporary users is less justified, shifting the constraint toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_goods_alternatives, empirical, 'Whether alternative incentive models obviate the copyright scaffold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 230).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccm_psr_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ccm_psr_tr_t40, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(ccm_psr_tr_t80, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(ccm_psr_tr_t120, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 120, 0.2).
narrative_ontology:measurement(ccm_psr_tr_t160, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 160, 0.25).
narrative_ontology:measurement(ccm_psr_tr_t200, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(ccm_psr_tr_t230, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 230, 0.35).

% Extraction over time
narrative_ontology:measurement(ccm_psr_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ccm_psr_be_t40, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(ccm_psr_be_t80, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement(ccm_psr_be_t120, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 120, 0.38).
narrative_ontology:measurement(ccm_psr_be_t160, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 160, 0.45).
narrative_ontology:measurement(ccm_psr_be_t200, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(ccm_psr_be_t230, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 230, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(copyright_constitutional_mandate__public_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% The copyright constitutional mandate kernel decomposes into three structurally distinct readings: the public-scaffold reading (this file) which treats monopoly as temporary means to public domain enrichment; the corporate-enclosure reading which treats copyright as maximal property protection; and the judicial-ambiguity reading which treats term length as unreviewable legislative discretion. Each reading has a distinct epsilon, beneficiary structure, and normative telos.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
