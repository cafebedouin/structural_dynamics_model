% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money Origin: First-Held Reading
 *   domain: monetary/technological/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the 'first-held reading' of contested
 *   kernel digital_money_origin. The reading asserts that digital money
 *   emerged when individuals first held non-physical monetary instruments as
 *   practical stores of value—a practice-contingent origin date. This reading
 *   is structurally distinct from became_thinkable_reading (which dates
 *   origin to technical/institutional conceivability) and
 *   regulatory_recognition_reading (which dates origin to formal
 *   incorporation into monetary aggregates). The first-held reading
 *   privileges implementation and practice over conceptualization or
 *   regulatory acknowledgment; it makes adoption behavior the temporal
 *   marker. This makes the origin date later than became_thinkable (by
 *   definition—thought precedes practice), but earlier than
 *   regulatory_recognition (formal money-supply incorporation lagged
 *   widespread private holdings). The extraction profile is tangled because
 *   the first-held boundary simultaneously enables genuine coordination
 *   (borderless transfer, faster settlement, bearer instruments) and
 *   asymmetrically benefits those with infrastructure access while excluding
 *   those without, and requires active enforcement of the boundary itself
 *   (what counts as 'practical,' who has 'held,' how long, at what scale).
 *
 * KEY AGENTS:
 *   - Early adopters with infrastructure access (beneficiaries, moderate power, mobile exit)
 *   - Technology vendors and exchanges (beneficiary-agenda-setters, institutional, arbitrage exit)
 *   - Financial intermediaries (beneficiary-gatekeepers, institutional, constrained exit)
 *   - Unbanked populations (victims, powerless, trapped exit)
 *   - Low-infrastructure regions (victims, organized, constrained exit)
 *   - Legacy-currency-bound institutions (victims-resistors, organized, identity-locked exit)
 *   - Monetary authorities (observers, institutional, analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.68).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.45).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin: First-Held Reading").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary/technological/institutional").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, '804dc158-0863-42cc-b5b8-64c727874916').
narrative_ontology:cs_kernel_codification('804dc158-0863-42cc-b5b8-64c727874916', distributed).
narrative_ontology:cs_authority_grounding('804dc158-0863-42cc-b5b8-64c727874916', distributed).
narrative_ontology:cs_reading_relation('804dc158-0863-42cc-b5b8-64c727874916', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('804dc158-0863-42cc-b5b8-64c727874916', digital_money_origin__regulatory_recognition_reading, coexists_with).
narrative_ontology:cs_axiom('804dc158-0863-42cc-b5b8-64c727874916', foundational, practice_precedes_policy).
narrative_ontology:cs_axiom_status(practice_precedes_policy, holdable).
narrative_ontology:cs_axiom_grounding('804dc158-0863-42cc-b5b8-64c727874916', practice_precedes_policy, deontological).
narrative_ontology:cs_axiom('804dc158-0863-42cc-b5b8-64c727874916', foundational, implementation_as_evidence).
narrative_ontology:cs_axiom_status(implementation_as_evidence, holdable).
narrative_ontology:cs_axiom_grounding('804dc158-0863-42cc-b5b8-64c727874916', implementation_as_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('804dc158-0863-42cc-b5b8-64c727874916', inception_of_technical_feasibility).
narrative_ontology:cs_drift_state('804dc158-0863-42cc-b5b8-64c727874916', contemporary_institutional_incorporation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('804dc158-0863-42cc-b5b8-64c727874916', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters_with_infrastructure).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, technology_vendors).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, financial_intermediaries).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, low_infrastructure_regions).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, legacy_currency_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with access to computers, internet connectivity, and financial education who can adopt digital monetary instruments early. They gain lower transaction costs, borderless transfer capability, and portfolio optionality unavailable to those without infrastructure. Their exit path is trivial—they can revert to physical currency at any time.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters_with_infrastructure, beneficiary,
    moderate, biographical, arbitrage, global).

% Companies building wallets, exchanges, payment processors, and blockchain infrastructure. They capture transaction fees, data rents, and first-mover advantage in the ecosystem. They actively market adoption, shape technical standards, and define what counts as 'practical' digital money through their implementation choices.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, technology_vendors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, technology_vendors, agenda_setter).

% Banks and payment networks that integrate digital money into their services. Early participants gain market positioning and fee revenue; those entering late face coordination lock-in. They enforce interoperability standards and settlement rules that define the practical boundary of 'held' digital instruments.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, financial_intermediaries, beneficiary,
    institutional, generational, constrained, global).

% Individuals without bank accounts or devices capable of holding digital money. The first-held reading excludes them by construction: they cannot hold what requires infrastructure they lack. They bear the opportunity cost of exclusion from lower-transaction-cost channels and network effects that accrue to those with access.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_populations, payer,
    powerless, biographical, trapped, local).

% Geographic areas with spotty internet, limited power supply, or weak telecommunications backbone. The practical definition of 'first held' depends on implementation barriers these regions lack resources to overcome. They face either delayed inclusion (after infrastructure matures elsewhere) or vendor-mediated inclusion (at higher cost through satellite or expensive partnerships).
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, low_infrastructure_regions, payer,
    organized, generational, constrained, regional).

% Individuals and institutions whose practice, identity, and institutional arrangements are built around physical currency and traditional banking. They do not choose to adopt digital money despite infrastructure availability; their 'non-adoption' is treated as non-participation in the first-held definition. They bear increasing friction costs as commerce migrates to digital rails.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, legacy_currency_users, payer,
    organized, biographical, identity_locked, global).

% Central banks and financial regulators observe the transition from outside the coordination mechanism. They measure adoption rates, monitor stability risks, and deliberate whether to incorporate digital money into monetary aggregates (a choice that defines an alternative origin reading). Their authority matters because regulatory recognition can retroactively legitimize or invalidate the first-held boundary.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_authorities, observer,
    institutional, generational, analytical, national).

% The structural process by which digital money's value grows as more participants adopt it. This is a doctrinal entity, not an actor, but it is the mechanism through which early adopters benefit—the constraint's persistence rides on continued network expansion, which benefits those already inside.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, network_effects_mechanism, beneficiary,
    powerless, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(digital_money_origin__first_held_reading, network_effects_mechanism).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__first_held_reading, technology_vendors).
narrative_ontology:fixing_cost_class(digital_money_origin__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of value transfer and storage without physical intermediation: enables bearer instruments, reduces transaction friction, and creates a secondary payments layer independent of traditional banking infrastructure. Coordination is achieved through technical interoperability (shared ledgers, cryptographic proofs, message protocols) and network effects (value accrues as participation grows).
% TRANSFER_FUNCTION: Moves opportunity for value accumulation and transactional advantage from those dependent on physical currency and traditional banking to those with infrastructure access and early-adoption timing. Moves transaction fees and data rents from users to technology vendors and intermediaries. Moves network effects (liquidity, price discovery, settlement speed) to early participants who benefit from adoption by later arrivals.
% ABSENT_VOICES: Unbanked populations and low-infrastructure regions would object that the first-held boundary excludes them by construction; they would argue for infrastructure-contingent rather than practice-contingent origin dates. Legacy-bound institutions would object that the reading privileges a minority adoption pattern over embedded institutional practice. Monetary authorities would argue that the first-held reading pre-empts their authority to define what counts as money by making a technical/practical fact do regulatory work.
% DISAPPEARANCE_RATIONALE: If the first-held origin definition disappeared—replaced by became_thinkable or regulatory_recognition readings—the boundary of digital money's creation date would shift backward (shifting causality from practice to concept or forward to formal acknowledgment). The set of early beneficiaries would change, the allocation of first-mover rents would be redrawn, and the narrative of which populations 'participated' in the origin would be rewritten. The institutional clock on digital money's history would reset.
% FOUNDING_PROBLEM: Physical currency and traditional banking presented bottlenecks: transfer settlement took days, cross-border movement required intermediaries, individuals without bank accounts had no store of value. Digital technology made bearer instruments and instant settlement technically feasible.
% FOUNDING_PROBLEM_CORROBORATION: Technology companies and early adopters attest the founding problem was real and urgent. Monetary authorities attest the problem was already being managed within existing frameworks and the new capability was luxury, not necessity. Unbanked populations would attest the problem persists despite digital money (for them)—corroboration from outside benefiting parties supports the reading that digital money solved problems for a subset, not a universal problem.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the first-held reading creates a strict boundary: you either hold digital money or you do not, and holding requires infrastructure, knowledge, and often capital to acquire. Once the boundary is drawn, it becomes path-dependent—early adopters capture network effects, and later arrivals inherit the distribution of value that early adoption created. The extraction grows over the interval (0.38 → 0.68) because network effects compound: early adopters' holdings become more valuable as adoption spreads, and infrastructure vendors capture growing fee rents as the installed base expands. Suppression is moderate (0.45) because exclusion is partly technical (you cannot hold what infrastructure cannot support) and partly social (identity-locked users choose not to adopt despite access). Theater is low (0.22) because the coordination function (settlement speed, borderless transfer) is genuine and measurable; the performative component is moderate-sized marketing around the stability and legitimacy of instruments whose value depends on continued adoption. The temporal series shows the constraint hardening over time: as digital money becomes more embedded in financial plumbing, the first-held boundary becomes harder to revise retroactively. The rising theater_ratio reflects increasing focus on institutional legitimacy and regulatory narratives (central banks issuing press releases, compliance infrastructure expanding) as the constraint matures—a sign of institutionalization of what began as technical practice.
 *
 * PERSPECTIVAL GAP:
 *   Payer seats and agenda-setter seats diverge sharply because the reading makes practice the origin marker, which privileges the agenda-setter's agency. Technology vendors are agenda-setters: they define what is 'practical,' what counts as 'held,' what instruments are 'legitimate' by building the infrastructure and setting the standards. This gives them control over the origin boundary itself—they can retroactively claim to have created digital money by pointing to adoption they facilitated. Payer seats (unbanked, low-infrastructure, legacy-bound) have no such control; the boundary is imposed on them. The same constraint computes differently at each seat: at the vendor seat, it is a rope (genuine coordination, moderate enforcement, benefiting); at the unbanked seat, it is closer to snare (pure exclusion, high suppression, no exit).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries have low directionality (d ~ 0.2–0.3): early adopters and vendors benefit without being forced—they chose to adopt and shape the technology. Victims have high directionality (d ~ 0.7–0.9): unbanked populations are excluded by infrastructure they lack, low-infrastructure regions are excluded by geography and capital they do not control, legacy-bound users are trapped by institutional inertia and identity-lock even where infrastructure exists. The constraint's directionality is asymmetric by design: it enforces and perpetuates the advantage of those who adopted early over those who adopted late or cannot adopt. This asymmetry is what makes it tangled_rope rather than rope: genuine coordination is present (the technical layer solves real problems), but it is coupled to extractive asymmetry (the distribution of early-adoption advantage).
 *
 * MANDATROPHY ANALYSIS:
 *   The first-held reading avoids mandatrophy by tethering the origin date to practice rather than to a founding problem. The founding problem (settlement friction, banking exclusion) persists in some forms and is contested—monetary authorities and traditional intermediaries attest it was already managed within existing systems. The reading does not claim to solve the founding problem permanently; it claims to mark when the solution became practical. This prevents the mandate from being obsolete at origin. However, the reading is vulnerable to a different mandatrophy pathway: if the first-held boundary becomes so institutionalized and regulatory frameworks incorporate digital money so thoroughly that the boundary becomes invisible (becomes natural law rather than constructed constraint), the mandate will have drifted from 'mark the origin' to 'define the boundary,' and the constraint will persist not because the boundary is useful but because it is embedded. The measurement series rising theater_ratio is a signal of this drift: as institutionalization increases, the constraint's function shifts from coordination to legitimacy-maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practical_holder_boundary,
    'What counts as ''first held'' and ''practical''? Is it first transaction, minimum holdings, sustained holdings, or widespread holdings across a population?',
    'Empirical: examine historical records of early adopter holdings (blockchain history, exchange records, survey data) and establish a threshold for ''practical'' adoption. Conceptual: ask whether the reading endorses an ordinal threshold (first person to hold) or cardinal threshold (widespread enough to be useful).',
    'An ordinal threshold makes the origin date extremely early (first Bitcoin wallet, ~2009) and makes the reader responsible for declaring which holding counts; a cardinal threshold pushes the date later (when holdings were common enough to support regular transactions) and distributes agency across the network. Different thresholds produce different victim sets and beneficiary concentrations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(practical_holder_boundary, conceptual, 'Where the boundary between ''holding digital money'' and ''experimenting with it'' lies.').

omega_variable(
    infrastructure_access_asymmetry,
    'Is exclusion of unbanked and low-infrastructure populations structural (they literally cannot hold digital money without infrastructure) or policy-chosen (vendors could have built for low-bandwidth environments but chose not to)?',
    'Empirical: examine which populations cannot participate due to technical inability vs. due to vendor design choices. Historical counterfactual: could vendors have prioritized low-bandwidth, low-power digital money from inception?',
    'If structural, the constraint''s suppression is partly inevitable and the classification remains tangled_rope (coordination + unavoidable asymmetry). If policy-chosen, the suppression becomes fully extractive and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_access_asymmetry, empirical, 'Whether exclusion is a technical necessity or a result of vendor prioritization.').

omega_variable(
    network_effects_lock_in,
    'Do network effects in digital money create lock-in that prevents later adopters from capturing early-adopter rents, or do they distribute value broadly as adoption spreads?',
    'Empirical: compare holding profiles and transaction volumes across cohorts of adopters (early vs. late) and measure whether late adopters'' holdings appreciate similarly. Compare fee structures across time to see whether early adopters continue to enjoy lower costs than later arrivals.',
    'Strong lock-in supports tangled_rope classification (early adopters capture durable advantage through coordination). Broad distribution would move the constraint toward rope (coordination benefits are passed on). Current evidence points to lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_lock_in, empirical, 'Whether network effects lock in early-adopter advantage or diffuse it over time.').

omega_variable(
    kernel_reading_underspecification,
    'Which of the three kernel readings is this constraint really instantiating—does the first-held reading actually differ from the became_thinkable reading, or is the boundary between them ambiguous?',
    'Conceptual: examine whether ''conceivability'' and ''practical holdings'' can be separated temporally or whether early adopters were always already thinking-through-implementation, making the readings inseparable.',
    'If separable, three distinct constraint stories with different beneficiaries and extraction profiles. If inseparable, the kernel has only two genuine readings and one is redundant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underspecification, conceptual, 'Whether the first-held reading is genuinely distinct from became_thinkable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__first_held_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(digi_tr_t3, digital_money_origin__first_held_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(digi_tr_t6, digital_money_origin__first_held_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(digi_tr_t12, digital_money_origin__first_held_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(digi_tr_t18, digital_money_origin__first_held_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement(digi_tr_t25, digital_money_origin__first_held_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__first_held_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(digi_be_t3, digital_money_origin__first_held_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(digi_be_t6, digital_money_origin__first_held_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(digi_be_t12, digital_money_origin__first_held_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(digi_be_t18, digital_money_origin__first_held_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(digi_be_t25, digital_money_origin__first_held_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__first_held_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(digi_su_t3, digital_money_origin__first_held_reading, suppression_requirement, 3, 0.38).
narrative_ontology:measurement(digi_su_t6, digital_money_origin__first_held_reading, suppression_requirement, 6, 0.41).
narrative_ontology:measurement(digi_su_t12, digital_money_origin__first_held_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(digi_su_t18, digital_money_origin__first_held_reading, suppression_requirement, 18, 0.44).
narrative_ontology:measurement(digi_su_t25, digital_money_origin__first_held_reading, suppression_requirement, 25, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(digital_money_origin__first_held_reading, 0.18).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel digital_money_origin. Sibling readings (became_thinkable_reading and regulatory_recognition_reading) are separate constraint stories with different origin dates, beneficiary sets, and extraction profiles. All three readings share the same core phenomenon (digital money exists) but disagree on when emergence occurred and what counts as evidence of emergence. Network effects: regulatory_recognition_reading influences this reading by retroactively legitimizing the holdings this reading identifies as origin; became_thinkable_reading influences this reading by providing the conceptual precondition without which 'first held' could not occur.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__first_held_reading, powerless, 0.85).
constraint_indexing:directionality_override(digital_money_origin__first_held_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
