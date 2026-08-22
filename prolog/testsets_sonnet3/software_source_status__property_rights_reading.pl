% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Proprietary Software Licensing as Legitimate Property Right
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the property_rights_reading of the
 *   software_source_status kernel: source code is a proprietary asset created
 *   through labor and investment, and the creator (or the firm that employs
 *   the creator) holds a legitimate right to restrict access, copying, and
 *   modification through licensing, copyright, and technical measures such as
 *   DRM and closed binaries. Under this reading, users who acquire software
 *   acquire only the contractual rights the license grants them — a right to
 *   run the program under stated conditions, not an entitlement to inspect or
 *   alter its internals. The reading treats this restriction regime as a
 *   legitimate application of general property and contract law to a new kind
 *   of asset, not as an ethical injustice (as freedom_imperative_reading
 *   holds) or a mere development-methodology inferiority (as
 *   pragmatic_development_reading implies) or a context-dependent welfare
 *   calculation (as utilitarian_hybrid_reading treats it). The metrics
 *   authored here describe the arrangement's actual operation AS THIS
 *   READING'S OWN LIGHTS SEE IT: substantial coordination function (funding
 *   sustained engineering investment, enabling firms to recoup R&D costs,
 *   supporting commercial software markets) coupled with real extraction
 *   (lock-in rents, interoperability barriers, repair restrictions) that
 *   requires active legal enforcement (DMCA anti-circumvention, EULA
 *   litigation, patent assertion) to sustain against erosion pressure from
 *   open alternatives and right-to-repair movements.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: Primary beneficiary (institutional/arbitrage) — collects license revenue, sets licensing terms, funds enforcement
 *   - venture_capital_investors: Secondary beneficiary (institutional/arbitrage) — captures returns contingent on defensible IP moats
 *   - end_users_locked_into_formats: Primary target (moderate/constrained) — bears switching costs and format lock-in
 *   - independent_repair_technicians: Target (powerless/trapped) — barred from accessing diagnostic code and repair manuals
 *   - downstream_academic_researchers: Target (moderate/constrained) — cannot inspect or verify proprietary algorithms used in published research claims
 *   - smaller_competing_developers: Target (moderate/constrained) — face patent thickets and closed APIs that raise entry costs
 *   - courts_and_legislatures: Analytical/enforcement observer (institutional/analytical) — adjudicates the boundary of legitimate restriction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.58).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.62).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Proprietary Software Licensing as Legitimate Property Right").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, 'f0d0ab24-b93e-495d-a04d-97d67bb99ec5').
narrative_ontology:cs_kernel_codification('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', distributed).
narrative_ontology:cs_authority_grounding('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', extraction).
narrative_ontology:cs_interpretation_layer_present('f0d0ab24-b93e-495d-a04d-97d67bb99ec5').
narrative_ontology:cs_reading_relation('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', foundational, labor_investment_grounds_exclusion_right).
narrative_ontology:cs_axiom_status(labor_investment_grounds_exclusion_right, holdable).
narrative_ontology:cs_axiom_grounding('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', labor_investment_grounds_exclusion_right, deontological).
narrative_ontology:cs_axiom('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', foundational, users_hold_contractual_not_inspection_rights).
narrative_ontology:cs_axiom_status(users_hold_contractual_not_inspection_rights, holdable).
narrative_ontology:cs_axiom_grounding('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', users_hold_contractual_not_inspection_rights, conventional).
narrative_ontology:cs_reference_frame('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', labor_desert_property_extension).
narrative_ontology:cs_drift_state('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', post_dmca_right_to_repair_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f0d0ab24-b93e-495d-a04d-97d67bb99ec5', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, venture_capital_investors).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, enterprise_software_publishers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, end_users_locked_into_formats).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_repair_technicians).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, downstream_academic_researchers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, smaller_competing_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, end_users_locked_into_formats).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, labor_desert_theory_of_ip).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, incentive_theory_of_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and own source code, set license terms restricting copying/modification/redistribution, and enforce those terms through litigation, DRM, and anti-circumvention law. Can restructure business models, relicense, or shift jurisdictions at will; the restriction regime is the vendor's own creation and chief revenue mechanism.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, proprietary_software_vendors, beneficiary).

% Fund software development on the expectation that IP restriction creates a defensible moat enabling returns. Their capital allocation decisions depend directly on the enforceability of source-code restriction; they can reallocate capital away from ventures if restriction weakens, giving them full exit mobility independent of any single firm's fate.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, venture_capital_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Purchase or license software under terms permitting only defined use, not inspection or modification. Benefit from stable, supported, professionally maintained products but bear switching costs when proprietary formats and interoperability barriers make migrating to alternatives expensive; exit is possible but costly, not foreclosed.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, end_users_locked_into_formats, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, end_users_locked_into_formats, beneficiary).

% Attempt to repair or diagnose devices running proprietary embedded software but are legally barred from accessing diagnostic codes, service manuals, or firmware under anti-circumvention provisions asserted by manufacturers. Have no path to legitimate access regardless of technical skill or customer demand; exit requires legislative right-to-repair intervention, not individual choice.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_repair_technicians, payer,
    powerless, biographical, trapped, national).

% Need to inspect proprietary algorithms (e.g., in published studies using commercial statistical or machine-learning software) to verify claims or replicate findings, but licensing terms prohibit reverse engineering or disclosure of internal behavior. Can sometimes substitute open-source tools but at cost to comparability with prior literature.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, downstream_academic_researchers, payer,
    moderate, biographical, constrained, global).

% Build competing products but face patent assertion, closed APIs, and interoperability barriers erected by incumbent vendors. Can build around some barriers through independent invention or open standards but absorb elevated legal risk and R&D cost as a condition of market entry.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, smaller_competing_developers, payer,
    moderate, biographical, constrained, global).

% Adjudicate the boundary between legitimate IP enforcement and anticompetitive or anti-repair overreach, through case law (DMCA circumvention suits, patent litigation) and legislation (right-to-repair statutes). Their rulings can expand or contract the restriction regime's legal footing over time.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables firms to recoup the cost of software development — which is expensive to create and costless to copy — by legally restricting unauthorized duplication and modification, thereby preserving the incentive to invest in building complex software in the first place.
% TRANSFER_FUNCTION: Moves license fees and lock-in switching costs from users, repair technicians, researchers, and smaller developers to the vendors and investors who hold the restricted IP, in exchange for access to (but not control over) the software.
% ABSENT_VOICES: Independent repair technicians and downstream researchers would object that restriction extends well past investment recoupment into pure lock-in and verification-blocking, but they hold no seat in licensing negotiations, which are set unilaterally by vendors and accepted via click-wrap agreements with no bargaining power on the user side.
% DISAPPEARANCE_RATIONALE: If source-restriction enforceability vanished overnight, the commercial proprietary software business model would collapse into a services/support model (as some open-source firms already operate), venture capital allocation into pure-software plays would shift dramatically toward hybrid or service-based models, repair technicians would gain immediate diagnostic access, and researchers could freely audit algorithms — a substantial reorganization of the software economy, not a null change.
% FOUNDING_PROBLEM: Software is costly to create but essentially free to copy once created; without legal restriction on copying and modification, developers and firms could not recoup development investment, threatening to undersupply complex software relative to social demand.
% FOUNDING_PROBLEM_CORROBORATION: Vendors and investors attest the problem remains fully live, citing continued high development costs for complex systems software and enterprise applications. Independent economists studying software markets (outside vendor employ) attest the problem is only partially live: development costs remain real, but network effects, first-mover advantage, and service/support revenue increasingly substitute for pure copy-restriction as recoupment mechanisms, and the restriction regime has expanded well beyond what recoupment alone would require (evidenced by DRM on abandoned/legacy software with no ongoing sales, and repair restrictions with no plausible copying-prevention rationale).
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects genuine coordination — proprietary licensing funds real engineering investment that a market with zero appropriability might underprovide — combined with a substantial rent component from lock-in, incompatible formats, and repair restriction that exceeds what's needed to recoup investment. Suppression (0.62) is higher than extraction because the restriction regime depends on active legal machinery: DMCA anti-circumvention provisions, EULA enforcement litigation, patent assertion, and technical DRM measures that must be continuously maintained and defended in court. Theater ratio is low-moderate (0.22) because most enforcement activity is functionally connected to genuine revenue protection, though a growing share (rising over the measured interval) defends lock-in mechanisms with no remaining security or coordination rationale. Accessibility collapse (0.45) is moderate rather than high: open-source and freeware alternatives exist and compete in many segments, so alternatives have not fully collapsed under this reading — that partial collapse is itself evidence this is not mountain-like natural law but a maintained institutional arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor/investor seat, this arrangement is a straightforward application of settled property law that happens to require enforcement the way any property right does (fences require maintenance; that doesn't make fencing extraction). From the repair technician or researcher seat, the same restriction regime denies functional access to something they have already paid for or need to verify, with no coordination benefit flowing back to them at all — pure cost with no offsetting collection. This is the seat divergence the tangled_rope classification is built to hold without collapsing into either pole.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software vendors and their investors are declared beneficiaries: they set licensing terms, collect revenue directly, and hold arbitrage-grade exit (they can restructure licensing models, jurisdiction-shop for favorable IP regimes, or pivot business models entirely) — this drives d toward the beneficiary end. End users, repair technicians, researchers, and competing developers are declared victims/payers: their exit options range from constrained (users can sometimes choose competing products but face switching costs) to trapped (repair technicians barred by law from accessing diagnostic interfaces regardless of preference) — this drives d toward the target end. The engine's directionality derivation should reflect that repair technicians sit nearer full-target than end users, given their categorically foreclosed legal access versus users' merely costly exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling firms to recoup software development investment that would otherwise be undercut by costless copying — remains partially live: development costs for complex software remain real and copying remains cheap. But the classification as tangled_rope (rather than pure rope) captures that the coordination function has been increasingly overlaid with extraction that exceeds recoupment: DRM outlives any plausible piracy-prevention rationale on legacy/abandoned software, repair restrictions serve no investment-recoupment purpose at all (a broken screen's repairability doesn't compete with the original sale), and patent thickets increasingly function to raise rivals' costs rather than protect genuine innovation. Classifying this as tangled_rope rather than snare acknowledges the coordination function is real and not merely cover; classifying it as tangled_rope rather than rope acknowledges the asymmetric extraction riding on that coordination is also real and requires active enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is source-code restriction a natural extension of general property rights, or a policy-contingent legal construct (copyright/patent/DMCA) that could have been assigned differently?',
    'Comparative legal history: jurisdictions and eras where software was treated as unprotectable (algorithms as math, pre-1980 U.S. copyright ambiguity) versus post-Diamond v. Diehr expansion. If protection tracks legislative choice rather than an inherent property in the artifact, the naturalness claim weakens.',
    'If source-restriction is discovered to be contingent legal policy rather than natural property extension, the property_rights_reading''s foundational premise shifts from a discovered fact to an authored institutional choice, undermining its self-presentation as the default/neutral reading against which alternatives argue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether property-in-code is natural-law-adjacent or a specific, revisable legislative artifact.').

omega_variable(
    coordination_vs_extraction_boundary_licensing,
    'How much of proprietary licensing revenue funds genuine coordination (R&D, support, security patching, interoperability testing) versus rent extracted purely from switching-cost lock-in?',
    'Cost-structure disclosure or comparative analysis against open-source projects with comparable feature parity and support quality, controlling for scale.',
    'A large lock-in-rent share supports classifying this reading''s real-world instantiation as tangled rope tipping toward snare; a small share supports a more rope-like reading of the same license structures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary_licensing, empirical, 'What fraction of licensing revenue is coordination cost versus lock-in rent.').

omega_variable(
    sibling_reading_framing_pressure,
    'Does treating users as ''consumers with contractual rights only'' understate structural dependencies (interoperability needs, repair rights, accessibility for disabled users) that a rights-based reading would recognize as more than contractual?',
    'Track right-to-repair litigation outcomes and accessibility-mandate carve-outs against proprietary licensing terms over time; a rising carve-out rate indicates the pure-contract framing is eroding under external pressure rather than being freely chosen by all parties.',
    'If carve-outs accumulate, this reading''s premise that restriction is purely a private contractual matter weakens, and downstream pressure toward the utilitarian_hybrid_reading''s mixed regime strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_framing_pressure, conceptual, 'Whether contractual framing survives external legal erosion via repair/accessibility mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t8, software_source_status__property_rights_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(soft_tr_t16, software_source_status__property_rights_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(soft_tr_t24, software_source_status__property_rights_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(soft_tr_t32, software_source_status__property_rights_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(soft_tr_t40, software_source_status__property_rights_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(soft_be_t8, software_source_status__property_rights_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(soft_be_t16, software_source_status__property_rights_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(soft_be_t24, software_source_status__property_rights_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(soft_be_t32, software_source_status__property_rights_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(soft_be_t40, software_source_status__property_rights_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(soft_su_t8, software_source_status__property_rights_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(soft_su_t16, software_source_status__property_rights_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(soft_su_t24, software_source_status__property_rights_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(soft_su_t32, software_source_status__property_rights_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(soft_su_t40, software_source_status__property_rights_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__property_rights_reading, 0.12).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the natural-language label 'software as intellectual property' per the ε-invariance principle. Each reading of the software_source_status kernel authors a distinct ε and distinct beneficiary/victim structure: property_rights_reading (this file, ε=0.58, tangled_rope) treats the standing restriction regime as substantially legitimate coordination with a real but bounded extraction overlay; freedom_imperative_reading would author a much higher ε for the same standing arrangement (treating nearly all restriction as illegitimate extraction, likely snare-leaning); pragmatic_development_reading brackets the rights question and would author ε based purely on development-quality outcomes; utilitarian_hybrid_reading would author a context-variable ε depending on market segment. All four are linked via affects_constraints rather than merged, because measuring 'the same' restriction regime through each reading's own lights yields genuinely different ε values — the hallmark of needing separate constraint files rather than one story with a hidden observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
