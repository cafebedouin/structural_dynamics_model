% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Software Source Code as Proprietary Asset (Property Rights Reading)
 *   domain: intellectual_property/technology_policy/political_economy
 *
 * SUMMARY:
 *   The property-rights reading of software source status asserts that source
 *   code is a legitimate intellectual property asset: creators and
 *   corporations that author software have the inherent right to restrict
 *   access to the code, modify it, use it, and charge for these rights. This
 *   is ONE reading of a contested kernel — the same underlying commitment to
 *   'what software is' that also sustains freedom-imperative,
 *   pragmatic-development, and utilitarian-hybrid readings. Under this
 *   reading, licensing restrictions (proprietary, GPL, copyleft, etc.) are
 *   legitimate exercises of ownership, users are customers/consumers with
 *   contractual rights only, and enforcement (legal, technical,
 *   institutional) is justified to prevent theft. The reading draws authority
 *   from intellectual property doctrine, economic incentive theory, and the
 *   concept of authorial control. It is institutionally dominant in
 *   commercial software and Western IP law.
 *
 * KEY AGENTS:
 *   - software_copyright_holders: Institutional authority; set the terms of use; collect licensing revenue
 *   - commercial_software_vendors: Powerful; benefit from lock-in and charging power; defend the property framing in policy
 *   - downstream_developers: Moderate power; constrained exit; bear cost of vendor dependencies they cannot modify
 *   - software_maintainers: Powerless; identity-locked to copyright holder's interests; face legal jeopardy for security work
 *   - users_with_interoperability_needs: Organized but constrained; cannot ensure long-term data accessibility across vendors
 *   - free_software_advocates: Excluded; would contest the framing but have no structural say in licensing policy
 *   - intellectual_property_authorities: Judicial/legislative observer seats; enforce the property-rights framing through law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.68).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.72).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Software Source Code as Proprietary Asset (Property Rights Reading)").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "intellectual_property/technology_policy/political_economy").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, 'cdc52959-8d78-49ff-ad10-62435ba08914').
narrative_ontology:cs_kernel_codification('cdc52959-8d78-49ff-ad10-62435ba08914', formalized).
narrative_ontology:cs_authority_grounding('cdc52959-8d78-49ff-ad10-62435ba08914', lineage).
narrative_ontology:cs_interpretation_layer_present('cdc52959-8d78-49ff-ad10-62435ba08914').
narrative_ontology:cs_reading_relation('cdc52959-8d78-49ff-ad10-62435ba08914', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('cdc52959-8d78-49ff-ad10-62435ba08914', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('cdc52959-8d78-49ff-ad10-62435ba08914', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('cdc52959-8d78-49ff-ad10-62435ba08914', foundational, proprietary_source_ownership).
narrative_ontology:cs_axiom_status(proprietary_source_ownership, holdable).
narrative_ontology:cs_axiom_grounding('cdc52959-8d78-49ff-ad10-62435ba08914', proprietary_source_ownership, deontological).
narrative_ontology:cs_axiom('cdc52959-8d78-49ff-ad10-62435ba08914', secondary, creator_incentive_hypothesis).
narrative_ontology:cs_axiom_status(creator_incentive_hypothesis, holdable).
narrative_ontology:cs_axiom_grounding('cdc52959-8d78-49ff-ad10-62435ba08914', creator_incentive_hypothesis, empirically_contingent).
narrative_ontology:cs_reference_frame('cdc52959-8d78-49ff-ad10-62435ba08914', authorial_intellectual_property_ownership).
narrative_ontology:cs_drift_state('cdc52959-8d78-49ff-ad10-62435ba08914', contemporary_software_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cdc52959-8d78-49ff-ad10-62435ba08914', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, software_copyright_holders).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, commercial_software_vendors).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, downstream_developers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, software_maintainers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, users_with_interoperability_needs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, commercial_software_vendors).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, intellectual_property_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, creator_incentive_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors, corporations, and rights-holding entities establish licensing terms, control source code access, and enforce restrictions through legal and technical means. They justify restrictions as necessary to protect their investment in creation and to maintain control over derivative works. They collect licensing revenue and royalties from use of the software.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from being able to charge for software and restrict modification, creating proprietary business models and competitive advantage through lock-in. Also pay licensing fees when integrating others' proprietary software, but this payment is a voluntary competitive choice rather than structural coercion. They actively defend the property-rights framing in policy and courts.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, commercial_software_vendors, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, commercial_software_vendors, payer).

% Cannot inspect, modify, or fork proprietary software they depend on. When building products that integrate proprietary libraries or platforms, they must accept whatever terms the copyright holder sets. They bear the cost of vendor lock-in, the inability to fix bugs or optimize for their use case, and the risk of license terms changing.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, downstream_developers, payer,
    moderate, biographical, constrained, global).

% Professional and volunteer maintainers of proprietary software operate under terms of service that restrict their ability to modify, redistribute, or study the code they maintain. Security researchers face legal jeopardy for disclosing vulnerabilities in proprietary systems. Their expertise is bound to the copyright holder's interests.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_maintainers, payer,
    powerless, biographical, identity_locked, global).

% Need to integrate software across vendor boundaries for business continuity, data portability, and system reliability. Proprietary licensing restricts their ability to modify adapters, maintain old systems after vendor abandonment, or ensure their data is accessible in perpetuity. They must accept whatever interoperability the vendor chooses to provide.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, users_with_interoperability_needs, payer,
    organized, biographical, constrained, global).

% Would argue that proprietary licensing restrictions are unjust barriers to knowledge sharing, collective improvement, and human agency over technology. They are structurally excluded from setting licensing policy and have only persuasive power to challenge the property-rights framing. The enforcement mechanism itself prevents their participation in the governed activity.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, free_and_open_source_advocates, excluded,
    organized, generational, trapped, global).

% Courts, patent offices, and legislative bodies enforce property-rights framing through copyright law, trade secret protection, and anti-circumvention statutes (DMCA, EUCD). They interpret the scope of creator rights and the boundary between fair use and infringement. They have structural authority to shift the constraint but face normative pressure to protect property.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, intellectual_property_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, software_copyright_holders).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides creators economic incentive to produce software by allowing them to restrict access and charge for use. Coordinates between authors (who receive revenue) and users (who receive functioning code under negotiated terms). Establishes clear property boundaries so rights are transactable and predictable.
% TRANSFER_FUNCTION: Moves licensing revenue and royalties from users, downstream developers, and integrators to copyright holders and commercial vendors. Restricts ability of maintainers and developers to modify or improve software, concentrating control authority in the hands of the copyright holder.
% ABSENT_VOICES: Free software advocates, users in jurisdictions with weak legal enforcement capacity, developers in non-English-speaking regions with limited access to proprietary documentation, and future maintainers of software after vendors abandon it. They are excluded from licensing negotiations and from the technical ability to participate in improving the code they depend on.
% DISAPPEARANCE_RATIONALE: If the property-rights framework disappeared overnight, the primary revenue model for commercial software would collapse; licensing would shift toward donation-based or service-based models; source code transparency would become default; derivative works and forks would proliferate; software ecosystems would reorganize around community contribution rather than vendor control. The entire commercial software industry's business model depends on the enforceability of this constraint.
% FOUNDING_PROBLEM: Creators of software needed economic incentive to invest time and resources in quality development. Without ability to restrict access and charge for use, individual authors and small firms would lack capital to build complex systems. The founding problem was: how to ensure software creators can recoup investment in R&D and maintain sustainable businesses.
% FOUNDING_PROBLEM_CORROBORATION: Commercial software vendors and intellectual property authorities attest the founding problem is live and ongoing — without protection they argue innovation would slow. Open source developers and economists studying software production attest the founding problem is substantially solved by alternative models (voluntary contribution, service revenue, corporate sponsorship); legislative testimony from technology scholars and empirical analysis of open source productivity outside this story's benefiting parties support the alternative reading.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).

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
 *   The metrics describe a constraint with high extractiveness (0.68 at present), high suppression (0.72), and moderate theater (0.42). Extractiveness is high because copyright holders collect licensing revenue and royalties decoupled from marginal service cost; the constraint extracts from downstream developers who cannot modify critical dependencies and from users locked into vendor platforms. Suppression is higher still because the constraint persists through active legal enforcement (DMCA, copyright law, trade secret statute), technical measures (DRM), and contractual terms that are backed by institutional power. Theater has risen over the interval (0.18 to 0.42) because justifications have shifted: early IP protection framed as incentivizing creation now coexists with network-lock-in arguments, anti-competitive behavior, and maintenance of market position — the functional activity is increasingly about control rather than incentive. The measurement series track a 51-year arc: from the early property-right era (1975, when software copyright was novel) through the rise of lock-in effects (1990s–2000s) to the contemporary platform era where the constraint's suppressive requirements have grown to maintain exclusive control at scale.
 *
 * PERSPECTIVAL GAP:
 *   The copyright-holder seat (institutional, agenda-setting, high exit options) experiences this as legitimate exercise of authorship and investment protection. The downstream-developer seat (moderate power, constrained exit) experiences it as vendor lock-in enforced by law. The maintainer seat (powerless, identity-locked) experiences it as legal jeopardy for security work and inability to improve code they maintain professionally. The IP-authority seat experiences it as coherent with property doctrine. The free-software-advocate seat (excluded) experiences it as systemic injustice. The engine computes these divergent directionalities from the structural data: institutional beneficiaries derive low d (near subsidy), powerless victims derive high d (near full extraction), excluded parties derive d from their inability to participate. These computed types are expected to diverge widely from the author's claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders have institutional power, arbitrage-grade exit (they can move between jurisdictions, business models, or licensing regimes), and global scope — they sit at the beneficiary end of the extraction spectrum (d near 0.0 to 0.2). Downstream developers have moderate power, constrained exit (cannot fork proprietary dependencies without legal and business risk), and global scope — they sit closer to the target end (d near 0.65 to 0.75). Maintainers are powerless, identity-locked (their professional credentials and reputation are bound to working on the software under the copyright holder's terms), and globally scoped — they sit at the full-target end (d near 0.85 to 0.95). Commercial vendors are powerful but partly captured by the constraint — they benefit from it but also pay licensing fees when integrating others' proprietary code. No directionality overrides are needed; the structural derivation from beneficiary/victim declarations and exit options captures the divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents a classic mandatrophy signal: the founding problem (creating economic incentive for software development) is substantially solved by alternative models (open source, corporate sponsorship, service revenue), yet the constraint persists and intensifies in suppression and theater. The founding problem status is authored as 'contested' because beneficiaries claim it remains live (without IP protection innovation would slow) while other parties attest the problem is solved or never required this solution. The disappearance verdict is 'world_rearranges' — the commercial software business model depends on enforceability. This creates the characteristic piton warning: a constraint whose founding mandate has atrophied but whose enforcement machinery has hardened and whose function has shifted from incentivizing creation (1975 era) to maintaining vendor lock-in and market control (2020s era). The rising theater_ratio (0.18 → 0.42) supports this: security, innovation, and creator protection remain as justifications, but the measured suppression and extraction suggest control is the primary function. The constraint is not yet a piton because beneficiaries still collect substantial rents and because the founding problem retains some argumentative force; but the drift direction toward piton is evident in the measurements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creator_incentive_empirical_sufficiency,
    'Is proprietary licensing empirically necessary to create sufficient economic incentive for high-quality software development, or do alternative models (open source, service revenue, corporate sponsorship) provide equivalent or superior incentive structures?',
    'Comparative historical analysis of software productivity and innovation in proprietary vs. open-source regimes; controlled case studies of same developers working under each model; economic analysis of Linux kernel, LLVM, Apache, Kubernetes development trajectories vs. proprietary equivalents.',
    'If alternatives provide equivalent incentive at lower extractive cost, the founding problem is solved and the constraint''s mandate has truly atrophied — strong mandatrophy signal. If proprietary licensing provides measurably superior incentive, the founding problem remains live and suppression is justified as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_incentive_empirical_sufficiency, empirical, 'Whether proprietary licensing is necessary for optimal software development incentive.').

omega_variable(
    forking_feasibility_counterfactual,
    'If copyright holders could not legally restrict source code modification and redistribution, would downstream developers and maintainers fork proprietary software to improve it, or would the constraint''s coordination function persist voluntarily?',
    'Regulatory experiments that permit forking or derivative works (EU right-to-repair legislation, software escrow provisions); analysis of fork rates and ecosystem health in permissively-licensed projects vs. proprietary equivalents; developer surveys on licensing preferences.',
    'If forking would occur at scale and improve software quality, the suppression is pure extraction with minimal coordination function — snare classification. If voluntary compliance persists even without legal enforcement, coordination function is real — tangled_rope classification sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(forking_feasibility_counterfactual, conceptual, 'Whether the constraint''s coordination function requires legal enforcement or persists voluntarily.').

omega_variable(
    reading_foreclosure_by_pragmatic_development,
    'Does the empirical superiority of open-source development methodology (pragmatic_development_reading) logically foreclose the property-rights reading, or do the two readings operate in different institutional contexts?',
    'Theoretical analysis: if pragmatic development shows open source superior for ALL software categories (systems, libraries, applications), the readings foreclose. If different contexts (safety-critical systems, consumer software, research tools) favor different models, readings coexist. Historical case studies of software where proprietary and open development models competed directly.',
    'If foreclosed, property-rights authority is logically invalidated and the constraint becomes indefensible on its own epistemic ground. If coexistent, both readings remain live and policy must adjudicate between them on normative grounds (fairness, welfare, freedom) rather than empirical grounds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_by_pragmatic_development, conceptual, 'Whether empirical methodology superiority of open development forecloses the property-rights reading.').

omega_variable(
    identity_lock_suppression_internalization,
    'For software maintainers and security researchers operating under identity-lock conditions, is the measured suppression primarily structural (legal jeopardy, contractual restriction, institutional power) or partially internalized (belief in IP rights, professional norm internalization, fear-induced compliance)?',
    'Post-exit trajectory analysis: if maintainers leaving the proprietary software ecosystem continue to respect intellectual property norms, suppression is partially internalized. If they immediately adopt different norms, suppression was structural. Interviews with former maintainers in jurisdictions without IP enforcement (lower-income countries, certain socialist systems) about whether they changed behavior after exiting IP-enforcing regimes.',
    'If substantially internalized, the effective suppression persists even in low-enforcement contexts and is harder to resolve through legal reform alone. If structural, legal reform (weakening IP enforcement) would enable rapid behavior change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_internalization, empirical, 'Degree to which maintainer suppression is internalized vs. structurally enforced.').

omega_variable(
    kernel_reading_committer_ground,
    'This is a kernel reading: which institutional/ideological commitment grounds the selection of the property-rights reading as authoritative (vs. the freedom-imperative, pragmatic, or utilitarian readings)? Is it intellectual property doctrine, creator incentive theory, economic efficiency arguments, or distributive power dynamics?',
    'Historical tracing of how property-rights framing became dominant in software licensing discourse; analysis of judicial decisions grounding copyright in property theory vs. incentive theory; policy analysis of whose interests are served by institutional adoption of each reading.',
    'If grounded primarily in creator incentive (empirical claim), the reading is vulnerable to empirical challenge. If grounded in property-doctrine principle (deontological), the reading is more resilient to empirical counter-evidence. If grounded in power dynamics (pragmatic institutional analysis), the reading''s authority is contingent on institutional maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ground, conceptual, 'The institutional/epistemic ground of this reading''s authority within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 1975, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1975, software_source_status__property_rights_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement_basis(soft_tr_t1975, observed).
narrative_ontology:measurement(soft_tr_t1990, software_source_status__property_rights_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement_basis(soft_tr_t1990, observed).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__property_rights_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement_basis(soft_tr_t2000, observed).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__property_rights_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement_basis(soft_tr_t2010, observed).
narrative_ontology:measurement(soft_tr_t2020, software_source_status__property_rights_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(soft_tr_t2020, observed).
narrative_ontology:measurement(soft_tr_t2026, software_source_status__property_rights_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(soft_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t1975, software_source_status__property_rights_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement_basis(soft_be_t1975, observed).
narrative_ontology:measurement(soft_be_t1990, software_source_status__property_rights_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement_basis(soft_be_t1990, observed).
narrative_ontology:measurement(soft_be_t2000, software_source_status__property_rights_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement_basis(soft_be_t2000, observed).
narrative_ontology:measurement(soft_be_t2010, software_source_status__property_rights_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement_basis(soft_be_t2010, observed).
narrative_ontology:measurement(soft_be_t2020, software_source_status__property_rights_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement_basis(soft_be_t2020, observed).
narrative_ontology:measurement(soft_be_t2026, software_source_status__property_rights_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(soft_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1975, software_source_status__property_rights_reading, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement_basis(soft_su_t1975, observed).
narrative_ontology:measurement(soft_su_t1990, software_source_status__property_rights_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement_basis(soft_su_t1990, observed).
narrative_ontology:measurement(soft_su_t2000, software_source_status__property_rights_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(soft_su_t2000, observed).
narrative_ontology:measurement(soft_su_t2010, software_source_status__property_rights_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement_basis(soft_su_t2010, observed).
narrative_ontology:measurement(soft_su_t2020, software_source_status__property_rights_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(soft_su_t2020, observed).
narrative_ontology:measurement(soft_su_t2026, software_source_status__property_rights_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(soft_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1975, tn=2026
narrative_ontology:measurement(soft_grid_01, software_source_status__property_rights_reading, accessibility_collapse(class), 1975, 0.38).
narrative_ontology:measurement(soft_grid_02, software_source_status__property_rights_reading, accessibility_collapse(class), 2026, 0.61).
narrative_ontology:measurement(soft_grid_03, software_source_status__property_rights_reading, accessibility_collapse(individual), 1975, 0.35).
narrative_ontology:measurement(soft_grid_04, software_source_status__property_rights_reading, accessibility_collapse(individual), 2026, 0.52).
narrative_ontology:measurement(soft_grid_05, software_source_status__property_rights_reading, accessibility_collapse(organizational), 1975, 0.42).
narrative_ontology:measurement(soft_grid_06, software_source_status__property_rights_reading, accessibility_collapse(organizational), 2026, 0.68).
narrative_ontology:measurement(soft_grid_07, software_source_status__property_rights_reading, accessibility_collapse(structural), 1975, 0.45).
narrative_ontology:measurement(soft_grid_08, software_source_status__property_rights_reading, accessibility_collapse(structural), 2026, 0.64).
narrative_ontology:measurement(soft_grid_09, software_source_status__property_rights_reading, resistance(class), 1975, 0.31).
narrative_ontology:measurement(soft_grid_10, software_source_status__property_rights_reading, resistance(class), 2026, 0.68).
narrative_ontology:measurement(soft_grid_11, software_source_status__property_rights_reading, resistance(individual), 1975, 0.22).
narrative_ontology:measurement(soft_grid_12, software_source_status__property_rights_reading, resistance(individual), 2026, 0.54).
narrative_ontology:measurement(soft_grid_13, software_source_status__property_rights_reading, resistance(organizational), 1975, 0.38).
narrative_ontology:measurement(soft_grid_14, software_source_status__property_rights_reading, resistance(organizational), 2026, 0.72).
narrative_ontology:measurement(soft_grid_15, software_source_status__property_rights_reading, resistance(structural), 1975, 0.35).
narrative_ontology:measurement(soft_grid_16, software_source_status__property_rights_reading, resistance(structural), 2026, 0.74).
narrative_ontology:measurement(soft_grid_17, software_source_status__property_rights_reading, stakes_inflation(class), 1975, 0.32).
narrative_ontology:measurement(soft_grid_18, software_source_status__property_rights_reading, stakes_inflation(class), 2026, 0.68).
narrative_ontology:measurement(soft_grid_19, software_source_status__property_rights_reading, stakes_inflation(individual), 1975, 0.28).
narrative_ontology:measurement(soft_grid_20, software_source_status__property_rights_reading, stakes_inflation(individual), 2026, 0.58).
narrative_ontology:measurement(soft_grid_21, software_source_status__property_rights_reading, stakes_inflation(organizational), 1975, 0.35).
narrative_ontology:measurement(soft_grid_22, software_source_status__property_rights_reading, stakes_inflation(organizational), 2026, 0.72).
narrative_ontology:measurement(soft_grid_23, software_source_status__property_rights_reading, stakes_inflation(structural), 1975, 0.41).
narrative_ontology:measurement(soft_grid_24, software_source_status__property_rights_reading, stakes_inflation(structural), 2026, 0.74).
narrative_ontology:measurement(soft_grid_25, software_source_status__property_rights_reading, suppression(class), 1975, 0.38).
narrative_ontology:measurement(soft_grid_26, software_source_status__property_rights_reading, suppression(class), 2026, 0.71).
narrative_ontology:measurement(soft_grid_27, software_source_status__property_rights_reading, suppression(individual), 1975, 0.32).
narrative_ontology:measurement(soft_grid_28, software_source_status__property_rights_reading, suppression(individual), 2026, 0.61).
narrative_ontology:measurement(soft_grid_29, software_source_status__property_rights_reading, suppression(organizational), 1975, 0.41).
narrative_ontology:measurement(soft_grid_30, software_source_status__property_rights_reading, suppression(organizational), 2026, 0.76).
narrative_ontology:measurement(soft_grid_31, software_source_status__property_rights_reading, suppression(structural), 1975, 0.45).
narrative_ontology:measurement(soft_grid_32, software_source_status__property_rights_reading, suppression(structural), 2026, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__property_rights_reading, 0.18).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel software_source_status. Four structurally distinct constraints emerge from different readings of what software is and who should control it: property_rights_reading (this file — source code as proprietary asset), freedom_imperative_reading (software freedom as ethical requirement), pragmatic_development_reading (open source as superior methodology), utilitarian_hybrid_reading (maximizing aggregate welfare via context-dependent licensing). These are not four perspectives on one constraint but four different constraints, each with different ε, different beneficiary/victim structures, different types. The kernel they read is the underlying commitment to 'what software is' — a commitment that cannot be uniquely satisfied; different authorities (copyright law, free software licensing, development communities, welfare economists) read it differently. Each reading carries its own constraint story with full structural data, measurements, and omega variables documenting the reading-specific ambiguities. They are networked via affects_constraints to show their interdependence: institutional adoption of the property-rights reading suppresses the freedom-imperative reading institutionally; empirical success of open-source development influences the pragmatic reading's plausibility; utilitarian analysis attempts to hold all readings in conversation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
