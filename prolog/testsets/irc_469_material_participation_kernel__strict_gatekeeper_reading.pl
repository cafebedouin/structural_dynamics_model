% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC §469 Material Participation Gatekeeper (Strict Interpretation)
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   IRC §469's material participation requirement is a gatekeeper that
 *   controls who can offset passive real estate losses against ordinary
 *   income. The strict interpretation instantiated here defines 'material
 *   participation' narrowly: at least 500 hours of work per year, regular and
 *   continuous participation, and primary business activity of the taxpayer.
 *   This reading emerged from the 1986 Tax Reform Act, which was a direct
 *   policy response to aggressive real estate tax shelters in the 1980s. The
 *   strict gatekeeper reading prioritizes preventing fraudulent loss
 *   deductions and protecting the tax base over enabling legitimate real
 *   estate professionals to deduct genuine operational losses. The constraint
 *   exhibits all six DR types depending on the observer's structural
 *   position. For the real estate professional unable to qualify, it is a
 *   snare: high suppression (they cannot exit), high extraction (loss
 *   deductions are forfeited), and minimal coordination function (the
 *   constraint provides no benefit to the trapped agent). For the IRS, it is
 *   rope: clear objective rules (500 hours, contemporaneous records) enable
 *   systematic verification. For the Treasury, it is rope: coordination of
 *   the tax base. For the 1986 legislative intent, it is piton: the
 *   regulation has become theatrical, enforced rigidly to the point where
 *   many legitimate operators cannot qualify despite genuine involvement. For
 *   the analytical observer, it risks becoming mountain: the appearance of an
 *   immutable law of tax administration. This constraint's theater_ratio
 *   (0.68) reflects that the documentation burden has grown beyond what is
 *   functionally necessary for fraud prevention—the real test is now
 *   primarily performative compliance with a 500-hour formality rather than
 *   verification of genuine operational involvement. The suppression
 *   trajectory (0.48 → 0.62) shows enforcement tightening over the interval
 *   as audit techniques improved and standards hardened.
 *
 * KEY AGENTS:
 *   - Real Estate Professional (Solo Proprietor): Primary victim (powerless/trapped) — unable to deduct losses even with genuine operational involvement; no exit short of abandoning the business
 *   - Organized Property Management Entity: Secondary victim (moderate/constrained) — can navigate strict standard with resources but faces significant compliance burden and friction
 *   - IRS Enforcement Authority: Primary beneficiary (institutional/arbitrage) — gains clear objective verification rules; experiences constraint as coordination tool
 *   - Tax Revenue Stream: Secondary beneficiary (institutional/arbitrage) — protected from artificial loss deductions; constraint coordinates tax base defense
 *   - 1986 Tax Reform Legislative Structure: Institutional actor (institutional/constrained) — enacted with genuine policy intent to prevent shelters; structure has become rigid and performative
 *   - Analytical Observer: Neutral (analytical/analytical) — risks naturalizing a 1986 policy choice as an immutable law of taxation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.58).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.62).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC §469 Material Participation Gatekeeper (Strict Interpretation)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'fa0ae731-6529-4db5-9480-ddcb3487dc83').
narrative_ontology:cs_kernel_codification('fa0ae731-6529-4db5-9480-ddcb3487dc83', formalized).
narrative_ontology:cs_authority_grounding('fa0ae731-6529-4db5-9480-ddcb3487dc83', extraction).
narrative_ontology:cs_interpretation_layer_present('fa0ae731-6529-4db5-9480-ddcb3487dc83').
narrative_ontology:cs_reading_relation('fa0ae731-6529-4db5-9480-ddcb3487dc83', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('fa0ae731-6529-4db5-9480-ddcb3487dc83', foundational, material_participation_requires_substantial_operational_involvement).
narrative_ontology:cs_axiom_status(material_participation_requires_substantial_operational_involvement, holdable).
narrative_ontology:cs_axiom_grounding('fa0ae731-6529-4db5-9480-ddcb3487dc83', material_participation_requires_substantial_operational_involvement, conventional).
narrative_ontology:cs_axiom('fa0ae731-6529-4db5-9480-ddcb3487dc83', foundational, loss_deduction_abuse_prevented_by_strict_documentation_standard).
narrative_ontology:cs_axiom_status(loss_deduction_abuse_prevented_by_strict_documentation_standard, holdable).
narrative_ontology:cs_axiom_grounding('fa0ae731-6529-4db5-9480-ddcb3487dc83', loss_deduction_abuse_prevented_by_strict_documentation_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('fa0ae731-6529-4db5-9480-ddcb3487dc83', strict_loss_deduction_gatekeeper).
narrative_ontology:cs_drift_state('fa0ae731-6529-4db5-9480-ddcb3487dc83', contemporary_regulatory_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa0ae731-6529-4db5-9480-ddcb3487dc83', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_enforcement_authority).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_income_dependent_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_revenue_stream).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professionals).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, active_operator_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_development_entities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REAL ESTATE PROFESSIONAL (SNARE) — A taxpayer who actively participates in real estate management, rehabilitation, or rental operations cannot deduct losses against ordinary income if they fail the strict material participation test. The documentation requirements are severe and asymmetrically enforced. No meaningful exit: the professional must either comply with expensive record-keeping or abandon loss deductions entirely. Pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strict_gatekeeper_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED PROPERTY MANAGER (TANGLED ROPE) — A property management entity with resources to maintain detailed daily logs, contemporaneous documentation, and professional support can navigate the strict standard but faces significant compliance costs. The constraint coordinates rule-of-law verification (prevents fraud in loss deduction claims) while extracting substantial compliance burden. Agency exists but is heavily constrained by documentation friction.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IRS ENFORCEMENT AUTHORITY (ROPE) — The strict material participation standard is a coordination mechanism for the IRS: it provides objective rules (hours worked, contemporaneous records) that enable systematic audit and loss deduction verification. The enforcement gain comes from clearer verification pathways. The IRS experiences the constraint as a coordination tool, not extraction.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strict_gatekeeper_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TAX REVENUE STREAM (ROPE) — From the Treasury perspective, the strict material participation requirement coordinates the defense of the tax base: it prevents high-income investors from shielding ordinary income through real estate losses. This is a net coordination function with asymmetric benefit distribution—high-income investors bear the compliance burden; tax revenue is protected.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strict_gatekeeper_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: 1986 TAX REFORM LEGACY (PITON) — IRC §469 was enacted in 1986 as a direct response to real estate tax shelter abuse. The strict material participation standard reflects genuine policy intent to close a loophole. Over time, however, the administrative apparatus for enforcement has become theatrical: the standard is now so rigid that many legitimate real estate professionals cannot qualify despite genuine operational involvement. The regulation persists through legislative inertia rather than adaptive function—it has become a ceremonial gate rather than a policy tool.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strict_gatekeeper_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a tax-economic perspective, some verification standard for active participation is inherent to any loss-deduction regime that seeks to distinguish legitimate operational involvement from passive investment with artificial losses. This perspective risks naturalizing the strict gatekeeper standard as a necessary law of tax administration. The engine will flag this as a false summit: the 'inherent need for verification' does not logically mandate the specific strict material participation test adopted in 1986.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strict_gatekeeper_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strict_gatekeeper_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strict_gatekeeper_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, TR),
    TR >= 0.70.

:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from real estate professionals in the form of forfeited passive loss deductions. The extraction is not complete (passive losses can be carried forward and eventually realized) but is substantial and asymmetric—high-income investors bear the burden while the Treasury gains protection. The value reflects that the strict standard is primarily extractive for the trapped agent, though it contains a genuine (if over-engineered) coordination function. Suppression (0.62): High. Multiple barriers prevent exit: (1) Compliance friction—documentation requirements are severe and asymmetrically enforced; (2) Career capital sunk in real estate operations—professionals cannot easily pivot to other industries; (3) No meaningful appeal mechanism—the 500-hour standard is legislated and inflexibly applied. Theater ratio (0.68): High and rising. The constraint's functional purpose (prevent artificial losses) could be achieved with substantially lower documentation burden. The 500-hour rule and contemporaneous record-keeping requirements have become primarily performative compliance rituals rather than necessary fraud prevention. The rise from 0.48 to 0.68 reflects increasing rigidity and formalism as IRS enforcement has tightened and real estate complexity has grown, making the documentation burden outstrip its functional value.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a classic perspectival inversion. The constraint originates as a coordination mechanism intended to protect the tax base (rope/institutional perspective—IRS, Treasury). Yet it transforms into a snare for the real estate professional who faces suppression and extraction with no exit. The gap reveals that beneficiaries (IRS, Treasury) perceive coordination; victims perceive extraction. The piton perspective identifies that the original policy intent has been overtaken by formalism—the 1986 legislative structure now exists primarily to maintain a verification ritual that has become decoupled from its functional purpose. The mountain perspective risks naturalizing what is actually a contingent 1986 policy choice, obscuring that the constraint could be reformed.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the constraint's extraction and coordination flows. The real estate professional (powerless/trapped) faces maximum extraction and suppression with no exit pathway—directionality d ≈ 0.92, producing high f(d) and high chi. The organized property manager (moderate/constrained) has sufficient resources to navigate compliance but at significant cost; directionality d ≈ 0.65, producing moderate chi. The IRS and Treasury (institutional/arbitrage) are beneficiaries with high exit capacity and benefit from coordination—directionality d ≈ 0.10, producing low or negative chi (the constraint subsidizes these agents). The 1986 structure (institutional/constrained) is partially captured by its own rigidity—it cannot easily adapt to changed circumstances; directionality reflects this constraint. The analytical observer (analytical/analytical) has no direct structural benefit or cost; directionality d ≈ 0.72 per canonical fallback, producing the moderate chi signature of analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the strict gatekeeper reading is structurally a tangled rope with a false summit risk. The 'inherent law' perspective (mountain) naturalizes a specific 1986 policy choice as an immutable requirement. The false summit detection fires because beneficiaries exist (IRS, Treasury) who benefit from the constraint's existence. The core mandatrophy is whether the strict standard's extraction is justified by genuine fraud prevention needs or is excess formalism. Omega variables 1, 2, and 3 provide the empirical and conceptual pathways to resolve this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_burden_vs_fraud_prevention_tradeoff,
    'What level of documentation burden is necessary and sufficient to prevent loss deduction fraud while maintaining legitimate real estate professional participation?',
    'Empirical analysis of audit outcomes: false-negative rates (fraudulent claims undetected) vs false-positive rates (legitimate professionals denied deductions). Comparative analysis of pre-1986 (lower documentation bar) vs post-1986 regimes.',
    'If strict standard is necessary for fraud prevention: constraint remains tangled_rope with extraction justified by coordination function. If lower burden would prevent 95%+ of fraud: strict standard is pure extraction mechanism (snare), not coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_burden_vs_fraud_prevention_tradeoff, empirical, 'Optimal tradeoff between documentation burden and fraud prevention').

omega_variable(
    material_participation_definition_kernel_contestation,
    'Is the strict material participation test (500+ hours, regular/continuous participation, primary business activity) the reading mandated by IRC §469, or is a more flexible substantial involvement standard consistent with the statute''s text and legislative history?',
    'Tax Court case evolution; IRS regulatory guidance consistency; congressional testimony and legislative history analysis for §469. Analysis of whether strict gatekeeper reading forecloses or coexists with strategic shelter reading.',
    'If strict reading is the only defensible statutory interpretation: reading forecloses strategic shelter reading. If legislative history permits both readings: readings coexist, and constraint classification depends on interpretive authority in power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(material_participation_definition_kernel_contestation, conceptual, 'Whether strict material participation test is mandated by statute or is one contestable reading').

omega_variable(
    real_estate_professional_status_heterogeneity,
    'Are real estate professionals with genuine operational involvement uniformly trapped by the strict standard, or do some segments (large developers, institutional operators, tax-sophisticated entities) possess sufficient resources and expertise to navigate compliance?',
    'Audit rate analysis by business size/sophistication. Deduction denial rate comparison: solo proprietors vs organized entities. Survey of compliance costs as percentage of claimed losses.',
    'If heterogeneous: powerless classification (solo proprietor perspective) is accurate, but organized segments experience tangled_rope or rope. If uniform: broad coalition of trapped agents possible, shifting powerless to organized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_estate_professional_status_heterogeneity, empirical, 'Heterogeneity of real estate professional compliance capacity').

omega_variable(
    passive_loss_carryforward_substitute_mitigation,
    'Do passive loss carryforward provisions (allowing unused losses to be deducted in later years when passive income is generated, or at disposition) provide sufficient mitigation of the strict material participation gate, converting the snare into a tangled_rope or rope?',
    'Actuarial analysis of loss realization timing: typical waiting period until passive income generation or property disposition. Tax burden analysis: discounted present value of deferred vs immediate deduction.',
    'If carryforward is functionally equivalent to immediate deduction: constraint is mitigated toward rope. If carryforward is substantially delayed or never realized: strict gate remains snare for most taxpayers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_loss_carryforward_substitute_mitigation, empirical, 'Efficacy of passive loss carryforward as mitigation mechanism').

omega_variable(
    strategic_shelter_reading_coexistence,
    'This constraint is one reading (strict gatekeeper) of a contested kernel. The sibling reading (strategic shelter) interprets the material participation requirement as a flexible safe harbor designed to encourage real estate development while preventing artificial tax shelters. Do these readings coexist in live dispute, or does the strict gatekeeper reading logically foreclose the strategic shelter interpretation?',
    'Analysis of Tax Court jurisprudence: do opinions simultaneously honor both stricter and more flexible interpretations depending on context, or has one reading achieved dominance? Legislative history: did Congress intend a single interpretation or permit judicial flexibility?',
    'If coexists_with: both readings are operative in different jurisdictions or during different policy eras; the constraint''s classification oscillates. If strict forecloses strategic: only one reading is defensible; constraint classification is stable across interpretive contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_shelter_reading_coexistence, conceptual, 'Logical relationship between strict gatekeeper and strategic shelter readings of material participation kernel').

omega_variable(
    false_summit_naturalization_risk,
    'The analytical perspective risks treating the strict material participation requirement as an immutable law of tax administration (''any loss-deduction system needs verification''). Is this naturalization justified, or does it obscure the 1986 policy choice that could be revised?',
    'Historical analysis: pre-1986 regimes used different participation standards (or none). Comparative tax law: how other jurisdictions (UK, Canada, Australia) handle loss deduction verification. Feasibility analysis: could a lower-burden standard achieve equivalent fraud prevention?',
    'If naturalization is unjustified: false summit signature fires; engine reclassifies from mountain to tangled_rope/snare. If there is inherent tax-administration constraint: mountain classification stands, but false summit still signals that beneficiaries exist and the constraint should be evaluated for extraction bias.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization_risk, empirical, 'Whether strict standard is immutable feature of tax administration or 1986 policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc469_strict_tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(irc469_strict_tr_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(irc469_strict_tr_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement(irc469_strict_tr_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(irc469_strict_be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(irc469_strict_be_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(irc469_strict_be_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(irc469_strict_be_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(irc469_strict_su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(irc469_strict_su_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(irc469_strict_su_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(irc469_strict_su_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_loss_limitation_tax_arbitrage).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professional_definition_boundary).

% DUAL FORMULATION NOTE:
% The IRC §469 material participation kernel has two structurally distinct readings: strict_gatekeeper_reading (this file, ε=0.58, tangled_rope with extraction predominant) and strategic_shelter_reading (sibling file, ε≈0.35, tangled_rope with coordination predominant). These readings share the same underlying regulatory text but differ in interpretive authority grounding and substantive threshold values. Both readings are linked via network.affects_constraints to downstream constraints (passive loss arbitrage, professional status boundaries). The two readings are not observable-dependent variants of a single constraint—they represent different epistemic commitments about what the statute permits, and they produce measurably different ε values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
