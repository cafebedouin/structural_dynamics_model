% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software Control Regime (Freedom-Imperative Reading)
 *   domain: political economy of technology / intellectual property
 *
 * SUMMARY:
 *   This story instantiates the freedom_imperative_reading of the
 *   software_control_legitimacy kernel: the standing arrangement under
 *   contest is the proprietary software regime — vendor-drafted licenses,
 *   withheld source, technical protection measures, and the legal machinery
 *   enforcing all three — assessed by this reading's own lights, in which
 *   control over one's computing is a fundamental right and every proprietary
 *   program imposes a categorical denial of it. Per the epsilon-referent
 *   rule, epsilon is authored for the standing arrangement, never for the
 *   free-software arrangement this reading endorses. The claim/metric gap is
 *   deliberate and small here: the reading CLAIMS snare and the authored
 *   metrics describe heavily extractive, actively enforced operation with
 *   identifiable victims — but the claim is authored from the categorical
 *   deontological seat, and the engine computes per-seat classifications from
 *   the structural data regardless. KEY AGENTS (by structural relationship):
 *   - proprietary_software_vendors: Primary agenda-setter
 *   (institutional/arbitrage) — drafts terms, runs enforcement, receives the
 *   gains - device_manufacturers: Secondary beneficiary
 *   (institutional/arbitrage) — rides locked platforms downstream, pays
 *   licensing upstream - proprietary_software_users: Primary target
 *   (powerless/constrained) — bears the transfer of money and control -
 *   enterprise_it_departments: Mitigated target (organized/constrained) —
 *   pays with negotiated concessions inside the arrangement -
 *   independent_repair_and_modding_community: Enforced-upon target
 *   (organized/trapped) — legal liability attaches to their core activity -
 *   free_software_developers: Excluded actor (organized/mobile) — operates
 *   the suppressed alternative from outside - legislators_and_courts:
 *   Analytical observer (institutional/analytical) — adjudicates the
 *   enforcement machinery's reach
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.88).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.78).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software Control Regime (Freedom-Imperative Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "political economy of technology / intellectual property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, 'bcb001fd-7e47-4fed-b3fc-d73273703c44').
narrative_ontology:cs_kernel_codification('bcb001fd-7e47-4fed-b3fc-d73273703c44', formalized).
narrative_ontology:cs_authority_grounding('bcb001fd-7e47-4fed-b3fc-d73273703c44', lineage).
narrative_ontology:cs_interpretation_layer_present('bcb001fd-7e47-4fed-b3fc-d73273703c44').
narrative_ontology:cs_reading_relation('bcb001fd-7e47-4fed-b3fc-d73273703c44', software_control_legitimacy__pragmatic_openness_reading, forecloses).
narrative_ontology:cs_reading_relation('bcb001fd-7e47-4fed-b3fc-d73273703c44', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('bcb001fd-7e47-4fed-b3fc-d73273703c44', software_control_legitimacy__commons_reading, forecloses).
narrative_ontology:cs_axiom('bcb001fd-7e47-4fed-b3fc-d73273703c44', foundational, user_computing_control_is_fundamental_right).
narrative_ontology:cs_axiom_status(user_computing_control_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('bcb001fd-7e47-4fed-b3fc-d73273703c44', user_computing_control_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('bcb001fd-7e47-4fed-b3fc-d73273703c44', secondary, proprietary_software_categorically_illegitimate).
narrative_ontology:cs_axiom_status(proprietary_software_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('bcb001fd-7e47-4fed-b3fc-d73273703c44', proprietary_software_categorically_illegitimate, deontological).
narrative_ontology:cs_reference_frame('bcb001fd-7e47-4fed-b3fc-d73273703c44', user_sovereignty_over_computing).
narrative_ontology:cs_drift_state('bcb001fd-7e47-4fed-b3fc-d73273703c44', saas_and_drm_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bcb001fd-7e47-4fed-b3fc-d73273703c44', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, device_manufacturers).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, independent_repair_and_modding_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, enterprise_it_departments).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, device_manufacturers).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, enterprise_it_departments).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, copyright_as_use_control_right).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, trade_secret_incentive_theory).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, clickwrap_assent_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the license terms under which their software may be run, withhold the source code, and operate the enforcement machinery: contract litigation, technical protection measures, and certification programs that condition hardware and partner status on compliance. License and subscription revenue accrues directly to them. They can selectively open components, relicense, or shift to service models when convenient, and several have done so for parts of their portfolios while keeping control points closed.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Ship hardware whose value depends on vendor-controlled firmware, signed boot chains, and licensed operating systems. Locked platforms protect their accessory ecosystems, service revenue, and warranty control. At the same time they pay per-unit licensing fees to operating-system vendors and must accept those vendors' compatibility requirements, so they sit one layer down in the same control chain they impose on their own customers.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, device_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, device_manufacturers, payer).

% Run programs whose inner workings they cannot inspect, whose behavior they cannot change, and which they may not copy or share beyond narrow license grants. They pay purchase prices and subscriptions, absorb forced feature changes and discontinuations, and depend on the vendor for security patches. Switching to free alternatives is possible for some tasks but carries compatibility costs, learning costs, and — on phones, cars, and appliances — often no option at all.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users, payer,
    powerless, biographical, constrained, global).

% Procure software at negotiated volume terms unavailable to individuals, receiving support contracts, audit defenses, and a single throat to choke when systems fail. They inherit deep dependency on vendor file formats and APIs, making migration projects multi-year risks. Their scale buys them concessions within the arrangement that individual users never see, but not exit from it.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, enterprise_it_departments, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, enterprise_it_departments, beneficiary).

% Modify, repair, and interconnect devices and programs as their trade and hobby. Anti-circumvention statutes and license prohibitions attach legal liability to the core activities of inspecting and altering protected code, and technical protection measures raise the skill floor continuously. They cannot stop modifying without ceasing to be what they are; their defensive options are exemption petitions and jurisdictional arbitrage.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, independent_repair_and_modding_community, payer,
    organized, biographical, trapped, global).

% Build and maintain a parallel body of software under licenses that guarantee inspection, modification, and redistribution rights. They are outside the proprietary arrangement's governance entirely — no seat in its standard-setting, no share of its revenue — and their interoperability work is periodically chilled when courts or statutes expand what counts as circumvention. Their exit is already taken: they operate the alternative ecosystem whose viability is the standing rebuttal to claims that the arrangement is unavoidable.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_developers, excluded,
    organized, generational, mobile, global).

% Decide the enforceability of license terms, the scope of anti-circumvention law, and the boundaries of statutory exemptions for repair, research, and archival use. They hear testimony from vendors, repair coalitions, libraries, and archives, and their rulings periodically redraw what the license terms can reach.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, legislators_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates capital for large-scale software development: turning programs into priced products gives firms a revenue line that funds teams, decade-scale maintenance, security response, and liability-backed support, and gives customers a single accountable vendor for complex systems.
% TRANSFER_FUNCTION: Moves license fees and subscription payments from software users to publishing vendors, and moves decision rights over use, modification, repair, and redistribution from users to the vendor; the source code and the authority over it remain on the vendor side of the transaction.
% ABSENT_VOICES: Users are absent from license drafting — terms arrive on a take-it-or-leave-it basis with no negotiation seat and no meaningful opportunity to read. Independent developers and repairers are absent from the legislative and standards processes where anti-circumvention rules are written. Future users are absent from decisions that bind hardware, formats, and data to vendor-controlled paths for decades.
% DISAPPEARANCE_RATIONALE: If proprietary licensing and its enforcement machinery vanished overnight, the commercial software economy would reorganize around service contracts, support businesses, dual-model offerings, and public funding; locked bootloaders and DRM would lose their legal backing overnight; the installed base of proprietary formats and dependent enterprises would need migration paths. Computing continues, but the industry's revenue architecture and its control architecture both rearrange.
% FOUNDING_PROBLEM: Making software development commercially sustainable once general-purpose computers made copying nearly free: how does a firm recoup development cost when the product reproduces at zero marginal cost?
% FOUNDING_PROBLEM_CORROBORATION: Vendor trade associations attest the funding problem is live and solvable only through exclusive control. Outside the benefiting parties: the software-engineering economics literature on open-source sustainability attests the funding problem is real but partially solvable through support contracts, foundations, and dual licensing; public funders (European Commission NGI, NLnet, sovereign technology funds) act on the premise that non-proprietary models sustain development at scale. No corroborating source outside the vendor set attests that exclusive control is the unique solution.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.88) because the reading's categorical premise puts the entire proprietary class in scope: the transfer is not priced exchange but the removal of use, modification, and sharing rights, and it intensified as computing became ubiquitous (phones, cars, appliances extended the regime past the desktop). Suppression (0.78) is a raw structural property, unscaled by power or scope: it reflects the enforcement stack — copyright scope stretched to use-control, clickwrap contract, anti-circumvention statute, DRM — that the regime's persistence requires. Theater (0.40) captures the consent ritual: 'agreements' presented as bargains that no party reads or negotiates. Accessibility_collapse (0.55) is honestly middling: free alternatives exist and are viable for much of the workload, but secure-boot chains, driver availability, and format lock-in collapse the alternative path for whole device categories. Resistance (0.65) is high and organized: the free software movement, copyleft licensing, and right-to-repair coalitions constitute a standing counter-institution, which is itself evidence the arrangement is constructed rather than natural. The temporal series run on one shared grid (seven points, 1983–2024) with all three metrics authored at every point; the suppression series is included because enforcement-capacity change IS the traced dynamic — the visible step between 1997 and 2004 corresponds to the anti-circumvention statute era, an enforcement ratchet rather than drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the vendor seat the arrangement is ordinary commerce plus a coordination function it funds: capital concentration, maintenance accountability, a single responsible party — the engine should compute low effective extraction and a rope-or-tangled flavor there. From the user seat the same structure operates as categorical denial with constrained exit — high effective extraction, snare-flavored. The enterprise seat sits between: negotiated concessions dampen experienced extraction without touching the underlying control transfer. The excluded developer seat experiences the arrangement primarily as suppression of an alternative rather than as a fee. These divergences are computed from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: vendors (beneficiary, agenda-setter, arbitrage exit) sit near the full-beneficiary end; device manufacturers (beneficiary with arbitrage, but paying licensing fees upstream) sit slightly higher; users and the repair/modding community (victims, constrained-to-trapped exit) sit near the full-target end, with trapping pushing the repair community further toward full target than mere constraint pushes ordinary users. Enterprise IT derives as a payer with organized power and constrained exit — high but below individual users, reflecting their purchased mitigations. Free software developers are excluded rather than declared in either structural array, so their directionality falls to the canonical fallback; substantively they bear a chill effect (interoperability work exposed to circumvention liability) placing them well above symmetric, but the override surface keys on power atom rather than agent, and an override at 'organized' would also strike enterprise IT — so the residual is documented here and left to the derivation chain rather than mis-keyed. Observers are analytical and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy lens guards against two mislabelings. First, accepting the vendor coordination story wholesale would classify the regime as rope — but the structural data show the coordination function is inseparable from an enforced control transfer with named victims, which is the snare signature, and the freedom reading's categorical premise denies the coordination story any legitimating force regardless. Second, treating the arrangement as vestigial would classify it as piton — but the founding problem (funding development at zero marginal reproduction cost) is contested-live, enforcement capacity has ratcheted UP over the interval, and gains concentrate demonstrably in a named seat: nothing here is inertial performance. Accordingly mandatrophy is NOT resolved, no sunset clause exists, and the R5 mismatch read (status=contested x verdict=world_rearranges) correctly declines to fire the zombie flag: the arrangement persists because it is actively maintained and profitable, not because anyone forgot to dismantle it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is ONE reading (freedom_imperative_reading) of the kernel software_control_legitimacy. What would each sibling reading change structurally if adopted as the operative classification?',
    'Cross-reading comparison across the four sibling stories in the constraint family: align referent (the standing proprietary arrangement), then diff beneficiary/victim sets, epsilon, and computed type per reading.',
    'property_rights_reading would move vendors from beneficiaries to legitimate rights-holders and shrink the victim set to infringers, collapsing epsilon toward exchange-pricing levels; pragmatic_openness_reading would treat closure as a methodology choice, dropping epsilon sharply and recomputing toward rope/tangled_rope; commons_reading would dissolve the categorical victim set, allocating harm by governance quality rather than openness. The snare verdict is reading-indexed, not kernel-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story instantiates one of four readings; sibling adoptions produce materially different structural data.').

omega_variable(
    disagreement_location_moral_status_of_control,
    'Where exactly do the four readings disagree? Hypothesis: on the moral status of user control over computing — whether it is a deontological right (this reading), a creator-side property entitlement (property_rights_reading), an outcome-weighted methodology consideration (pragmatic_openness_reading), or a collectively negotiable governance parameter (commons_reading).',
    'Locate each sibling''s foundational axiom and test which structural element (victim set membership, epsilon level, enforcement legitimacy) moves when that axiom is granted or denied.',
    'If the disagreement is located in moral status rather than empirical facts, no amount of outcome data resolves the kernel contest; classification pluralism across the family is permanent and the family should be analyzed as competing normative frames over one referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_moral_status_of_control, conceptual, 'The specific structural element on which sibling readings diverge.').

omega_variable(
    freedom_harm_uniformity,
    'Does the freedom-denial this reading attributes to proprietary software hold uniformly across all proprietary software, or does it vary by segment (boot firmware and embedded controllers versus desktop productivity applications versus SaaS services)?',
    'Segment-level analysis: for each class, measure substitutability by free alternatives, practical modifiability, and the coupling between the proprietary component and user agency.',
    'If harm is uniform-categorical, the single epsilon of 0.88 is stable and the flat story stands. If harm varies widely by segment, this story fails epsilon-invariance at segment granularity and must decompose into a family of per-segment stories with distinct epsilon values linked by network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_harm_uniformity, empirical, 'Whether the categorical victim-set claim survives segment-level variation in freedom-denial intensity.').

omega_variable(
    eula_consent_validity,
    'Is click-through license assent morally and structurally efficacious consent — converting the license fee into a freely agreed price — or is it consent theater performed on terms no user reads or negotiates?',
    'Comprehension and deliberation studies of license acceptance behavior; comparative analysis of markets where negotiated enterprise terms exist versus consumer click-through.',
    'If assent is efficacious, part of the measured transfer is priced exchange rather than imposition, lowering effective extraction for consenting seats and weakening the snare reading toward tangled_rope. If assent is theatrical, the theater_ratio series measures a real consent deficit and the categorical reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eula_consent_validity, conceptual, 'Whether the regime''s consent rituals carry normative weight or are performative cover.').

omega_variable(
    suppression_source_ambiguity,
    'How much of the measured suppression is legal coercion (copyright scope, EULA enforceability, anti-circumvention statute, DRM) versus market lock-in (network effects, format incompatibility, hardware pairing) that would persist if the legal machinery were repealed?',
    'Natural experiments where legal bars were lifted: statutory exemption cycles for repair and interoperability research, jurisdictions with weaker enforcement — observe whether modification and repair activity flourishes when only the legal bar is removed.',
    'If lock-in dominates, the suppression scalar overstates the legal-coercion component and the regime''s persistence would survive legal reform, shifting analysis toward structural lock-in remedies; if legal coercion dominates, statutory reform is the load-bearing fix and the enforcement-ratchet trajectory is the critical dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Structural versus legal composition of the regime''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 1983, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1983, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1983, 0.08).
narrative_ontology:measurement(soft_tr_t1990, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(soft_tr_t1997, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1997, 0.2).
narrative_ontology:measurement(soft_tr_t2004, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2004, 0.26).
narrative_ontology:measurement(soft_tr_t2011, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2011, 0.31).
narrative_ontology:measurement(soft_tr_t2018, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2018, 0.37).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(soft_be_t1983, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1983, 0.66).
narrative_ontology:measurement(soft_be_t1990, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1990, 0.71).
narrative_ontology:measurement(soft_be_t1997, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1997, 0.75).
narrative_ontology:measurement(soft_be_t2004, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2004, 0.79).
narrative_ontology:measurement(soft_be_t2011, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2011, 0.83).
narrative_ontology:measurement(soft_be_t2018, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2018, 0.86).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1983, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1983, 0.34).
narrative_ontology:measurement(soft_su_t1990, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1990, 0.41).
narrative_ontology:measurement(soft_su_t1997, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1997, 0.49).
narrative_ontology:measurement(soft_su_t2004, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2004, 0.6).
narrative_ontology:measurement(soft_su_t2011, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2011, 0.67).
narrative_ontology:measurement(soft_su_t2018, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2018, 0.73).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: 'software control legitimacy' is a colloquial label covering four structurally distinct claims, each authored as its own story over the SAME referent (the standing proprietary arrangement) with reading-indexed epsilon. This story is the freedom_imperative_reading instance: categorical deontological premise, maximal victim set, epsilon 0.88. The pragmatic and property-rights siblings derive sharply lower epsilon from the same referent by denying the categorical premise; the commons sibling dissolves the categorical victim set into governance-quality variation. Upstream/downstream: the property_rights and pragmatic readings currently dominate legal and commercial discourse conditions, shaping what enforcement this reading's referent can deploy; this reading exerts pressure back by supplying the rights-based vocabulary that repair and interoperability litigation borrows. All four files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
