% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right (Property Rights Reading)
 *   domain: economic/technological/philosophical
 *
 * SUMMARY:
 *   Software control (who decides how code can be used, modified, copied, and
 *   distributed) is contested across technology policy, open-source
 *   communities, and commercial software. This constraint story instantiates
 *   THE PROPERTY-RIGHTS READING: the claim that software creators have a
 *   legitimate property right to restrict use and modification to protect
 *   investment and enable commercial sustainability. Under this reading, FOSS
 *   advocates, derivative developers, and end-users demanding
 *   interoperability are VICTIMS—denied the right to fork, redistribute, or
 *   modify software they depend on. Vendors and investors are
 *   BENEFICIARIES—they capture returns from licensing restrictions. This is
 *   NOT a claim that the property-rights reading is true or false; it is a
 *   structural model of the constraint AS THAT READING UNDERSTANDS IT.
 *   Sibling readings (freedom-imperative, pragmatic-openness, commons)
 *   instantiate DIFFERENT constraints with DIFFERENT beneficiary/victim
 *   structures from this one. Do NOT fold alternative readings into this
 *   story. Each reading generates a separate constraint story with its own ε,
 *   its own stakeholders, and its own classification; the stories are linked
 *   via network.affects_constraints to indicate they arise from the same
 *   kernel dispute.
 *
 * KEY AGENTS:
 *   - software_vendors: institutional power; set and enforce licensing terms; control code access and modification rights; collect license revenue; highly mobile exit
 *   - venture_investors: powerful; benefit from property-rights framing enabling valuations; arbitrage mobility; fund vendors with expectation of licensing revenue
 *   - foss_advocates: organized power globally; denied right to modify or fork popular software; legally threatened; bear direct suppression; constrained exit due to lock-in and network effects
 *   - derivative_developers: moderate power; prevented from building on closed software without licensing negotiation; constrained exit—depend on popular frameworks
 *   - end_users_demanding_interoperability: powerless; trapped in proprietary formats; cannot switch without losing data; immediate time horizons; denied control over own computing
 *   - software_employees: moderate power; benefit from employment funded by licensing revenue; restricted in what they can build/publish; identity-locked to vendor's restrictions
 *   - policy_makers: institutional power; set legal infrastructure (copyright, patent, trade-secret law); can rebalance through interoperability mandates or antitrust; neutral observers
 *   - competing_software_platforms: powerful; excluded from interoperability with dominant competitors' closed infrastructure; would compete on functionality if admitted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.58).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.62).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right (Property Rights Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "economic/technological/philosophical").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, 'bd508902-2271-46c7-9f53-76a6b17d3b0e').
narrative_ontology:cs_kernel_codification('bd508902-2271-46c7-9f53-76a6b17d3b0e', fixed_text).
narrative_ontology:cs_authority_grounding('bd508902-2271-46c7-9f53-76a6b17d3b0e', extraction).
narrative_ontology:cs_interpretation_layer_present('bd508902-2271-46c7-9f53-76a6b17d3b0e').
narrative_ontology:cs_reading_relation('bd508902-2271-46c7-9f53-76a6b17d3b0e', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('bd508902-2271-46c7-9f53-76a6b17d3b0e', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd508902-2271-46c7-9f53-76a6b17d3b0e', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('bd508902-2271-46c7-9f53-76a6b17d3b0e', foundational, intellectual_property_is_property).
narrative_ontology:cs_axiom_status(intellectual_property_is_property, holdable).
narrative_ontology:cs_axiom_grounding('bd508902-2271-46c7-9f53-76a6b17d3b0e', intellectual_property_is_property, deontological).
narrative_ontology:cs_axiom('bd508902-2271-46c7-9f53-76a6b17d3b0e', foundational, creator_investment_protection_justifies_restriction).
narrative_ontology:cs_axiom_status(creator_investment_protection_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('bd508902-2271-46c7-9f53-76a6b17d3b0e', creator_investment_protection_justifies_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('bd508902-2271-46c7-9f53-76a6b17d3b0e', copyright_and_property_protection_framework).
narrative_ontology:cs_drift_state('bd508902-2271-46c7-9f53-76a6b17d3b0e', contemporary_open_source_dominance, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bd508902-2271-46c7-9f53-76a6b17d3b0e', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, venture_investors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, closed_source_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, derivative_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, end_users_demanding_interoperability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_employees).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, software_employees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Companies that build proprietary software and enforce licensing restrictions. They set licensing terms, pursue infringement litigation, and lobby for copyright extension and anti-circumvention law. They collect license fees and subscription revenue directly, justifying restrictions as necessary to protect R&D investment and fund ongoing development.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Fund software ventures on the expectation that proprietary control enables exit valuations and licensing revenue. They benefit from copyright law, trade secret protection, and patent enforcement that undergird the property-rights framing. Their returns depend on the constraint's stability.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, venture_investors, beneficiary,
    powerful, biographical, mobile, global).

% Communities and individuals who believe software should be freely modifiable and distributable. Under the property-rights reading, they are prevented from modifying or forking popular software, prohibited from re-distributing their improvements, and legally threatened if they circumvent locks. Their labor and creativity are constrained by the licensing regime.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, biographical, constrained, global).

% Developers who want to build on existing software libraries, frameworks, or tools but are restricted by licensing terms. They must negotiate commercial licenses, reverse-engineer, or build duplicative tools from scratch. Their options are bounded by the vendor's licensing choices.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, derivative_developers, payer,
    moderate, biographical, constrained, global).

% Users locked into proprietary formats and APIs who cannot switch to competing software without losing data or functionality. They bear lock-in costs and are denied control over their own computing environment. Their exit is technically and legally blocked.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, end_users_demanding_interoperability, payer,
    powerless, immediate, trapped, global).

% Engineers and designers employed by proprietary software firms. They benefit from stable employment funded by licensing revenue but are restricted in what they can build, share, or publish. Their professional identity is bound to the vendor's restrictions.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_employees, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, software_employees, payer).

% Legislators and regulators who enforce copyright, trade secret, and patent law. They set the legal infrastructure that makes the property-rights reading enforceable. They can alter terms through legislation or antitrust intervention.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, policy_makers, observer,
    institutional, generational, analytical, national).

% Vendors whose business models depend on interoperability and platform openness. Under the property-rights reading, they are excluded from building on competitors' closed infrastructure. They have no voice in standard-setting that affects them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, competing_software_platforms, excluded,
    powerful, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates investment incentives: creators capture returns from their work by controlling reproduction and distribution; this enables sustained funding for software development without relying solely on public goods provision or patronage models. Provides quality assurance through unified development and maintenance of codebases.
% TRANSFER_FUNCTION: Moves economic value from users and derivative developers (who must license to use or extend software) to vendors (who collect licensing fees and subscription revenue). Also moves creative labor: FOSS developers' improvements and innovations are captured by vendors under restrictive licenses and cannot be widely shared without permission.
% ABSENT_VOICES: Users in jurisdictions without FOSS infrastructure, developing-world developers denied access by high licensing costs, competing vendors whose interoperability ambitions are blocked, and future developers who cannot build on existing proprietary work. These parties would object to the property-rights framing but are structurally excluded from software licensing negotiations and standard-setting.
% DISAPPEARANCE_RATIONALE: If software control ceased to be enforceable as property (copyright collapse, open-source licensing triumph, or regulatory breakup mandating interoperability), the software economy would reorganize: venture funding models would shift toward sponsorship and service models, proprietary vendors would compete directly with open-source peers on functionality and support rather than lock-in, lock-in costs would vanish enabling switching, and development incentives would operate through patronage, public goods funding, reputation, and employment rather than license revenue.
% FOUNDING_PROBLEM: Early software development required sustained investment in engineering talent and infrastructure. Copyright and trade-secret law created a mechanism to capture returns: restrict copying, charge for licenses, fund R&D from license revenue. This enabled companies to profitably build complex software without public subsidy or volunteer-only labor models.
% FOUNDING_PROBLEM_CORROBORATION: Software vendors and investors attest the problem remains live and the proprietary licensing solution remains necessary for funding complex software development. Open-source projects and economic studies from outside the vendor class attest that the problem is substantially solved by alternative funding models (sponsorship, public funding, service revenues), that the current restriction scope produces lock-in and interoperability costs exceeding coordination benefits, and that the founding problem no longer justifies the constraint's current enforcement scope globally.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   EXTRACTIVENESS (0.58): Moderate-high. The property-rights reading extracts value from derivative developers and users who want interoperability or control—they must pay licensing fees or accept constraints. However, the extraction is framed as compensation for intellectual property investment, not pure coercion, which keeps ε below pure-snare levels. The temporal series shows extraction rising from 0.38 to 0.58 over 20 observed years (t=0 to t=20), then plateauing around 0.58-0.59 in projections. This trajectory reflects increasing enforcement scope (lock-in mechanisms, anti-interoperability legal threats) while the founding justification (funding R&D) becomes less central to enforcement activity. SUPPRESSION (0.62): Moderate-high. The constraint requires active enforcement—litigation, copyright/DMCA claims, technical locks, lobbying for stronger IP law, EULA enforcement. Suppression is not universal (users who accept licensing terms face none) but substantial against violators. The temporal series shows suppression rising from 0.48 to 0.64 over 20 years, reflecting intensifying enforcement infrastructure and expanding anti-circumvention law. Projected suppression plateaus at 0.62-0.64 after t=25, suggesting enforcement enforcement infrastructure has matured. THEATER_RATIO (0.28): Moderate-low. The development/maintenance justification for restrictions is real (funded R&D, professional support, quality assurance), but theater is rising (from 0.12 at t=0 to 0.29 at t=30): enforcement activity increasingly targets derivative uses (preventing forks, blocking re-distribution, locking data formats) that pose no burden on maintenance. This is classic mandatrophy theater: the constraint persists past its coordination function and must defend itself with theatrical maintenance of the original justification. ACCESSIBILITY_COLLAPSE (0.71): Moderate-high. Once a developer or user understands they are locked into proprietary software, alternatives appear to collapse. Lock-in is real (switching costs are high, data formats are closed, network effects favor dominant platforms) or made true by licensing terms (legal barriers to reverse engineering, contractual restrictions on data portability). In some markets (mobile OSes, enterprise software, consumer platforms) alternatives are genuinely rare. RESISTANCE (0.68): Moderate-high. The constraint faces substantial organized resistance from FOSS communities, derivative developers, interoperability advocates, and policy makers in some jurisdictions (EU Digital Markets Act, right-to-repair movements, antitrust actions). Resistance is not strong enough to overthrow the constraint globally, but it is significant, growing, and producing policy countermeasures (forced interoperability mandates, right-to-repair law, antitrust breakups).
 *
 * PERSPECTIVAL GAP:
 *   FROM THE VENDOR'S SEAT: The constraint is genuine coordination and fair compensation. A vendor's R&D team builds software; they deserve returns on their investment; licensing fees enable quality assurance and sustained maintenance; users benefit from unified development and professional support. The vendor experiences the constraint as enabling prosperity and enabling the software ecosystem. DIRECTIONALITY: ~0.0 (full beneficiary). FROM THE FOSS ADVOCATE'S SEAT: The constraint is oppression and labor extraction. They are prevented from using their skills to build on software they care about; their improvements cannot be shared; their agency is denied by licensing restrictions. They experience the constraint as a barrier to creative freedom and collaborative development. DIRECTIONALITY: ~1.0 (full target). FROM THE DERIVATIVE DEVELOPER'S SEAT: The constraint is a licensing gate and extraction barrier. They want to build on popular frameworks but must negotiate commercial licenses, reverse-engineer, or build duplicative tools. DIRECTIONALITY: ~0.8 (mostly target). FROM THE POLICY MAKER'S SEAT: The constraint is an equilibrium between investment incentives and user autonomy. They see vendors capturing returns and FOSS advocates demanding freedom; they attempt to balance both. DIRECTIONALITY: ~0.5 (symmetric). The engine computes each seat's classification from power/exit/scope and these structural differences. The claimed type (tangled_rope) reflects the vendor's framing; the metrics reflect what is measurable from outside all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   BENEFICIARY SEATS: software_vendors (institutional, arbitrage exit, benefits directly—d ~0.0), venture_investors (powerful, mobile exit, benefits through valuations—d ~0.15). PAYER SEATS: foss_advocates (organized, constrained exit due to lock-in, bears suppression—d ~0.82 with override), derivative_developers (moderate power, constrained exit, bears licensing costs—d ~0.8), end_users (powerless, trapped exit, locked in—d ~0.95). MIXED SEATS: software_employees (moderate power, constrained exit, benefits from employment but pays through restricted creative agency—secondary_role split; d ~0.45 split). EXCLUDED SEATS: competing_platforms (powerful, mobile exit, excluded from the coordination itself—d ~0.65, not fully target because their exit is more mobile than trapped agents). OBSERVER SEATS: policy_makers (institutional, analytical exit—d = 0.5 by definition). The directionality_overrides entry corrects the automatic derivation for organized-power agents: without override, an organized payer would derive to ~0.5 because organized power is high; with override, it is corrected to ~0.82 because organized power in the payer role does not reduce their exposure to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   FOUNDING PROBLEM: Software development requires capital investment in engineering talent and infrastructure. Proprietary licensing with copyright/trade-secret enforcement solved this—venture funding unlocked, R&D teams staffed, complex software systems built. FOUNDING_PROBLEM_STATUS: Contested. VENDOR CLAIM: The founding problem is still live (open-source depends on volunteer labor and fragile patronage; proprietary investment is more reliable and produces higher-quality software). EXTERNAL CORROBORATION: Open-source projects (Linux, Apache, Kubernetes, LLVM) demonstrate sustained, funded development through alternative models (corporate sponsorship, foundation patronage, consulting services, public contracts). Economic research shows patent-backed VC funding is correlated with venture liquidity events but not with software quality or innovation rate—alternative metrics correlate more strongly with actual development outcomes. MANDATROPHY SIGNAL: The temporal measurements show theater_ratio rising (0.12→0.29, +141%) while foundational coordination problem (sustainable R&D) is increasingly solved by alternative models (FOSS sponsorship, public funding). Enforcement activity increasingly targets derivative uses (preventing forks, blocking re-distribution, locking formats) that pose no burden on maintenance. This is the classic mandatrophy pattern: the constraint persists past its function and must defend itself with theatrical maintenance. MANDATROPHY_RESOLVED: False. The constraint has not been formally superseded—policy makers have not uniformly mandated interoperability or abolished copyright for software; vendors continue to argue forcefully that the founding problem justifies the constraint; the constraint remains actively enforced globally. However, mandatrophy IS IDENTIFIED: the evidence shows the founding function is substantially solved while enforcement scope expands. This enters the corpus as an UNRESOLVED mandatrophy candidate, signaling to policy analysis that the constraint's justification and its actual operation have diverged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    investment_incentive_necessity,
    'Is proprietary control structurally necessary to generate sufficient investment in software development, or do alternative models (open-source sponsorship, public funding, patronage) provide adequate incentive?',
    'Comparative empirical analysis: measure R&D spending, innovation rate, and developer compensation across proprietary and open-source ecosystems over 10-year intervals; survey venture-capital funding thresholds and the licensing models that actually trigger capital allocation.',
    'If alternatives provide adequate incentive, the property-rights reading''s justification weakens and the constraint reclassifies from tangled_rope (coordination+extraction) to pure snare (extraction without genuine coordination). If proprietary control is necessary, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_incentive_necessity, empirical, 'Whether proprietary restriction is necessary or merely sufficient for software funding.').

omega_variable(
    lock_in_cost_vs_coordination_benefit,
    'Do the coordination benefits of proprietary control (unified development, consistent user experience, funded maintenance) exceed the lock-in costs (inability to switch, no user control, interoperability barriers) when measured at the constraint''s current scope?',
    'Economic welfare analysis comparing user switching costs against measured benefits (uptime, feature parity, security). Compare proprietary and open-source software users'' reported satisfaction and cost of exit.',
    'If lock-in costs exceed coordination benefits, the constraint''s classification remains tangled_rope but mandatrophy emerges—the coordination function is overshadowed by extraction. If benefits exceed costs, tangled_rope is justified and stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_cost_vs_coordination_benefit, empirical, 'The balance between coordination value and lock-in harm.').

omega_variable(
    interoperable_property_rights_boundary,
    'Is the property-rights reading compatible with mandatory interoperability and data portability, or does interoperability logically foreclose the restriction-of-use authority that grounds the reading?',
    'Regulatory experiment: jurisdictions that mandate data portability and API interoperability while preserving copyright show whether restricted-use and interoperable-format can coexist, or whether interoperability forces functional openness incompatible with the reading.',
    'If interoperability and property rights coexist, the reading remains stable. If interoperability forecloses the reading''s authority structure, then the foundational axiom breaks and the reading moves from holdable to overridden within its own framework (the EU Digital Markets Act provides empirical evidence here).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interoperable_property_rights_boundary, conceptual, 'Whether the property-rights reading''s core premise is compatible with forced interoperability.').

omega_variable(
    kernel_reading_divergence,
    'This constraint instantiates the PROPERTY_RIGHTS_READING of the contested kernel software_control_legitimacy. How do the beneficiary/victim structures differ across the four sibling readings? Which reading''s beneficiaries become victims in another reading?',
    'Cross-reading structural audit: for each of the four readings (this one, freedom_imperative, pragmatic_openness, commons), identify the beneficiary and victim sets and measure the structural role inversions.',
    'Maps the kernel dispute onto a beneficiary/victim inversion graph. If all four readings agree on beneficiaries/victims but disagree only on whether the constraint is justified (claim divergence), the kernel dispute is normative. If beneficiary/victim sets invert across readings, the kernel dispute is structural (different constraints, different referents).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural location of the kernel dispute across readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__property_rights_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__property_rights_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__property_rights_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__property_rights_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__property_rights_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__property_rights_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(soft_tr_t35, software_control_legitimacy__property_rights_reading, theater_ratio, 35, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__property_rights_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__property_rights_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__property_rights_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__property_rights_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__property_rights_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__property_rights_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(soft_be_t35, software_control_legitimacy__property_rights_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__property_rights_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__property_rights_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__property_rights_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__property_rights_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(soft_su_t25, software_control_legitimacy__property_rights_reading, suppression_requirement, 25, 0.64).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__property_rights_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(soft_su_t35, software_control_legitimacy__property_rights_reading, suppression_requirement, 35, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__property_rights_reading, 0.18).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the software_control_legitimacy kernel. All four readings (property_rights, freedom_imperative, pragmatic_openness, commons) are instantiated as separate constraint stories with distinct beneficiary/victim structures and ε values. They are linked via affects_constraints to indicate kernel kinship. Each reading models the kernel as that reading's own framework understands it; no reading's model is subsumed or overridden by another. Sibling readings differ on: (1) Whether control is a property right (this reading) or a commons question (commons), fundamental user freedom (freedom_imperative), or development methodology choice (pragmatic_openness); (2) Who bears the costs—FOSS advocates under this reading, software vendors under freedom_imperative; (3) Whether the constraint coordinates development or extracts from users. The kernel dispute is located in the legitimacy frame, not in measurable facts about software development.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__property_rights_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
