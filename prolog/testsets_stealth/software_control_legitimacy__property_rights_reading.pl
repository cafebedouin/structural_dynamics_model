% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right (Property-Rights Reading)
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the property-rights reading — of
 *   the contested kernel software_control_legitimacy: who legitimately
 *   controls software, creators or users. The standing arrangement under
 *   contest is the operative proprietary-control regime (copyright-backed
 *   licenses, EULAs, DRM, anti-circumvention law, platform terms), assessed
 *   by this reading's own lights: because the reading holds creator
 *   restriction to be a legitimate property exercise, it authors moderate
 *   rather than maximal extractiveness, while acknowledging that the
 *   arrangement restricts user freedoms and leaves commons contributors
 *   without recourse. Per the epsilon-invariance principle, the sibling
 *   readings (freedom_imperative_reading, pragmatic_openness_reading,
 *   commons_reading) are separate constraints with their own files, epsilon
 *   values, and victim sets; this file links to them via
 *   network.affects_constraints. Claim/metric independence is preserved:
 *   claimed_type is tangled_rope because the arrangement pairs a genuine
 *   coordination function (appropriability financing) with asymmetric
 *   extraction, while the metrics describe observed operation independently
 *   of that claim. Fixing_cost is prohibitive because any legislative
 *   weakening must unwind treaty obligations (TRIPS, WIPO) against
 *   concentrated incumbent lobbying, for benefits that are diffuse across
 *   users — a cost class far above the benefit for any single fixer.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: Agenda-setter and primary beneficiary (institutional/arbitrage) — drafts license terms, operates enforcement, collects license and subscription revenue
 *   - venture_capital_investors: Beneficiary (powerful/arbitrage) — the property frame converts code into ownable, exit-ready assets
 *   - foss_advocates: Primary target (organized/identity_locked) — commons labor absorbable without reciprocity; freedom commitments constitute identity
 *   - end_users: Target with incidental coordination benefit (powerless/constrained) — pays license and subscription costs, bears restricted use, modification, and sharing rights
 *   - security_researchers: Excluded voice (moderate/constrained) — anti-circumvention liability chills auditing
 *   - technology_policy_analysts: Analytical observer (analytical/analytical) — no operational stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.54).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.64).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right (Property-Rights Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "economic/technological/legal").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, '07ed6ad0-988e-482b-a821-d8977473f2e4').
narrative_ontology:cs_kernel_codification('07ed6ad0-988e-482b-a821-d8977473f2e4', fixed_text).
narrative_ontology:cs_authority_grounding('07ed6ad0-988e-482b-a821-d8977473f2e4', lineage).
narrative_ontology:cs_interpretation_layer_present('07ed6ad0-988e-482b-a821-d8977473f2e4').
narrative_ontology:cs_reading_relation('07ed6ad0-988e-482b-a821-d8977473f2e4', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('07ed6ad0-988e-482b-a821-d8977473f2e4', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('07ed6ad0-988e-482b-a821-d8977473f2e4', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('07ed6ad0-988e-482b-a821-d8977473f2e4', foundational, creator_restriction_authority_is_legitimate).
narrative_ontology:cs_axiom_status(creator_restriction_authority_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('07ed6ad0-988e-482b-a821-d8977473f2e4', creator_restriction_authority_is_legitimate, deontological).
narrative_ontology:cs_axiom('07ed6ad0-988e-482b-a821-d8977473f2e4', secondary, exclusion_required_for_commercial_sustainability).
narrative_ontology:cs_axiom_status(exclusion_required_for_commercial_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('07ed6ad0-988e-482b-a821-d8977473f2e4', exclusion_required_for_commercial_sustainability, empirically_contingent).
narrative_ontology:cs_reference_frame('07ed6ad0-988e-482b-a821-d8977473f2e4', creator_exclusive_control_norm).
narrative_ontology:cs_drift_state('07ed6ad0-988e-482b-a821-d8977473f2e4', contemporary_open_source_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('07ed6ad0-988e-482b-a821-d8977473f2e4', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, venture_capital_investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, end_users).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, lockean_labour_desert_theory).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, incentive_argument_for_intellectual_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft end-user license agreements and platform terms, operate activation and digital-rights-management systems, litigate unauthorized copying and modification, and fund lobbying for treaty-level intellectual-property expansion. Collect license fees, subscription payments, and app-store commissions directly. When one control channel erodes they can relocate incorporation, convert products to hosted services, or adopt open-core structures, so their exposure to any single legal change is hedged.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Condition funding on defensible intellectual-property positions; the property frame is what turns code into an ownable, collateralizable asset with acquisition and public-offering exits. They pay nothing into the enforcement machinery directly and can reallocate portfolios across sectors whenever software returns compress.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, venture_capital_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Produce and maintain commons code, run foundations, and campaign for user freedoms and copyleft licensing. Under permissive licenses their contributions can be absorbed into proprietary products with no reciprocal claim, and the legal frame grants them no ownership stake in the ecosystem their unpaid labor sustains. Campaigning, foundation work, and the freedom commitment itself constitute their professional and ethical identity, so leaving the movement would mean abandoning work that defines them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, generational, identity_locked, global).

% Acquire software under terms forbidding modification, redistribution, and often repair or resale, and increasingly pay recurring subscriptions instead of one-time purchases. In exchange they receive professionally funded, maintained, and supported products. Switching to a competing product is possible, but remaining inside licensed software altogether is nearly unavoidable for work and daily life, so the terms are accepted rather than meaningfully chosen.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, end_users, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, end_users, beneficiary).

% Audit proprietary code for vulnerabilities; anti-circumvention statutes expose the act of inspection itself to liability, so findings go unreported, research moves to friendlier jurisdictions, or exemptions are sought case by case. They would argue that blanket restriction degrades security for every user, vendors included, but they hold no seat in license drafting or standards negotiations.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, security_researchers, excluded,
    moderate, biographical, constrained, global).

% Study the intellectual-property regime's effects on innovation rates, market concentration, and user welfare; publish empirical work on whether exclusion rights track their stated incentive rationale. They carry no operational stake in the arrangement's continuation or removal.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, technology_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the appropriability problem in software production: when copies are costless, a seller cannot recoup up-front development investment, so enforceable exclusion lets firms finance development, commit to support and update obligations, and plan multi-year products around recoverable costs.
% TRANSFER_FUNCTION: Moves license fees, subscription payments, and platform commissions from end users to proprietary vendors and their investors; moves uncompensated value from commons contributors when permissively licensed work is absorbed into proprietary products; moves decision rights over running, modifying, and sharing code from users to rights-holders.
% ABSENT_VOICES: Security researchers chilled by anti-circumvention liability, right-to-repair advocates, and purchasers in low-income markets priced out by per-seat licensing had no seat in license drafting or in the trade negotiations (WIPO, TRIPS) that set the regime's terms; the negotiating table was composed almost entirely of rights-holding industries.
% DISAPPEARANCE_RATIONALE: Commercial software as currently financed depends on recoupment through exclusion: remove it overnight and up-front-funded product development loses its repayment mechanism, firms pivot to hosting, support, and service revenue, pricing and product categories reshuffle, and the stock of legally modifiable code grows sharply. Software continues to be written, but the industry's financing structure reorganizes around the loss.
% FOUNDING_PROBLEM: In the 1970s and 1980s, hobbyist copying threatened the nascent packaged-software industry: a buyer could duplicate a program for nothing, so selling software looked unrecoverable and investment in products stalled. The property-rights framing was constructed to solve that appropriability crisis.
% FOUNDING_PROBLEM_CORROBORATION: Law-and-economics scholarship and industry history corroborate from outside the benefiting parties that the original appropriability crisis was real and that early firms genuinely withheld or failed over it. Independent empirical work on open-source and service-based production, together with vendors' own migration to subscriptions, contests whether the founding problem still justifies the current scope of control; no neutral party attests that it binds unchanged.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.54 (moderate): real transfers occur — license rents decoupled from marginal cost, one-time purchases converted into perpetual subscriptions, uncompensated absorption of commons labor — but bounded by genuine service value delivered (funded development, support, security response). Suppression is 0.64, authored as a raw unscaled structural property: persistence depends on legal machinery (anti-circumvention statutes, contract law, activation systems) that actively closes modification and redistribution routes; the mechanism is structural, not internalized. Theater ratio is 0.32: 'protecting innovation' rhetoric increasingly outruns the incentive rationale it cites (term extensions, platform commissions), yet enforcement still performs real appropriability work, so performance is a minority share. Accessibility_collapse is 0.5: free and open-source stacks keep alternatives partly alive, and understanding the arrangement does not collapse exit because open substitutes genuinely cover growing shares of the stack. Resistance is 0.6: copyleft enforcement, right-to-repair legislation, interoperability rulings, and persistent unauthorized copying meet the regime continuously. All three tracked series share one grid ({0,10,20,30,40,50}); the extractiveness series is deliberately non-monotonic — a 2006-era peak of binary-plus-DRM dominance, a dip as open source won the infrastructure layer and hosted delivery moved recoupment away from copy control, then recovery as subscriptions and platform commissions rebuilt rent extraction on the same legal frame. The suppression series tracks enforcement-capacity history: build-up through the DMCA era, partial substitution by architectural self-enforcement, then stabilization. Coordination type is resource_allocation: the arrangement's primary function is allocating returns to software investment, with moderate complexity and inherent transaction costs; the type-default floor is used, no override.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat experiences the arrangement as the enabling condition of its existence — from inside, it is the coordination structure that makes commercial software possible at all. The payer seats experience the same structure as enforced restriction: users accept terms they did not choose, and commons contributors watch their work appropriated without reciprocity. Investors experience it as asset integrity. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors sit near the beneficiary end: declared beneficiary, arbitrage-grade exit hedges any single channel. Investors likewise: pure beneficiaries with portfolio mobility. FOSS advocates sit near the full-target end: declared victims whose identity_locked exit amplifies effective extraction — they cannot leave the fight without abandoning constitutive commitments. End users derive mid-high: victim declaration plus constrained exit pushes toward target, moderated by the secondary beneficiary position and the real service value received. Security researchers are excluded rather than coordinated — no flow either way; the analyst seat carries no directional weight.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two opposite errors. Reading the arrangement as pure extraction erases the genuine appropriability function that finances commercial software — the founding problem was real and is corroborated by scholarship outside the benefiting parties. Reading it as pure coordination erases the asymmetric extraction — platform commissions, perpetual subscriptions, uncompensated commons absorption — that concentrates gains in the vendor and investor seats. On the genealogy interview the founding problem is contested rather than dead, so no zombie flag fires: the arrangement has not wholly outlived its function. But the temporal record shows extraction accumulating faster than the functional justification broadens, which is precisely the tangled-toward-snare pressure the measurement series exists to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the software_control_legitimacy kernel (reading: property_rights_reading); the freedom_imperative, pragmatic_openness, and commons readings instantiate different constraints over the same topic — which reading''s constraint should govern evaluation of a given software-control arrangement?',
    'Corpus-level comparison of the four sibling stories: divergent victim sets, epsilon values, and computed types locate the disagreement structurally rather than adjudicating it substantively.',
    'Under the freedom_imperative reading the same arrangement computes as a snare with users as victims; under pragmatic_openness it approaches a rope; classification is indexical to the reading, not to the topic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Classification is valid for this reading only; sibling readings are separate constraints.').

omega_variable(
    foss_victim_status_ambiguity,
    'Are FOSS advocates genuine victims of the property-rights arrangement, or voluntary participants in a parallel mode of production whose losses are opportunity costs rather than imposed harm?',
    'Trace concrete transfers: permissive-license absorption of commons code into proprietary products without reciprocity, and legal exposure of copyleft-adjacent practices; measurable uncompensated transfers establish structural victim status.',
    'If the losses are opportunity-cost-only, the victim set shrinks to end_users, epsilon falls toward rope territory, and the tangled_rope reading overstates the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foss_victim_status_ambiguity, empirical, 'Whether the FOSS victim declaration reflects imposed transfers or parallel-track opportunity cost.').

omega_variable(
    appropriability_necessity,
    'Is exclusionary control structurally necessary to fund commercial-grade software, or can service, patronage, and bounty models scale to replace it?',
    'Natural experiment across segments: compare sustained funding and product quality of hosted, open-core, and hybrid models against purely license-based models as service delivery matures.',
    'If replaceable, the coordination function weakens, excess extraction over the coordination floor grows, and the arrangement drifts toward snare; if necessary, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriability_necessity, empirical, 'Whether the coordination function genuinely requires exclusionary control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swc_prop_rights_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(swc_prop_rights_tr_t10, software_control_legitimacy__property_rights_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(swc_prop_rights_tr_t20, software_control_legitimacy__property_rights_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(swc_prop_rights_tr_t30, software_control_legitimacy__property_rights_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(swc_prop_rights_tr_t40, software_control_legitimacy__property_rights_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(swc_prop_rights_tr_t50, software_control_legitimacy__property_rights_reading, theater_ratio, 50, 0.32).

% Extraction over time
narrative_ontology:measurement(swc_prop_rights_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(swc_prop_rights_be_t10, software_control_legitimacy__property_rights_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(swc_prop_rights_be_t20, software_control_legitimacy__property_rights_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(swc_prop_rights_be_t30, software_control_legitimacy__property_rights_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(swc_prop_rights_be_t40, software_control_legitimacy__property_rights_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(swc_prop_rights_be_t50, software_control_legitimacy__property_rights_reading, base_extractiveness, 50, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(swc_prop_rights_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(swc_prop_rights_su_t10, software_control_legitimacy__property_rights_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(swc_prop_rights_su_t20, software_control_legitimacy__property_rights_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(swc_prop_rights_su_t30, software_control_legitimacy__property_rights_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(swc_prop_rights_su_t40, software_control_legitimacy__property_rights_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(swc_prop_rights_su_t50, software_control_legitimacy__property_rights_reading, suppression_requirement, 50, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, commons_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition: the colloquial debate 'should software be free or owned' conflates four structurally distinct claims, split here per the epsilon-invariance principle. This file is the property_rights_reading (moderate epsilon; vendors and investors benefit; FOSS advocates and end users bear costs). The freedom_imperative_reading authors high epsilon against the same standing arrangement with users as victims; pragmatic_openness_reading authors low epsilon treating control as methodology choice; commons_reading reframes the referent as collective governance. The property-rights reading is upstream of pragmatic_openness in one respect — copyleft enforcement borrows the property frame's own legal machinery — while standing in logical contradiction to freedom_imperative. Each member links to the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
