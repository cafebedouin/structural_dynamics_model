% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__freedom_imperative_reading, []).

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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Proprietary Software as Imposed Injustice (Freedom-Imperative Reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom-imperative reading of the
 *   software_source_status kernel: the claim that software freedom is a
 *   fundamental ethical requirement and proprietary software licensing is an
 *   injustice. Under this reading, all proprietary software is classified as
 *   an extractive snare — users are trapped in dependency on vendors who
 *   restrict their ability to audit, modify, and distribute code. The reading
 *   treats source-code access as an inalienable right, placing all users and
 *   restricted developers in the victim set. The constraint operates through
 *   legal enforcement (copyright law, DMCA, licensing contracts) and
 *   technical suppression (DRM, obfuscation, copyright restrictions on
 *   reverse-engineering). The founding problem (creating incentives for
 *   software development) is recognized as dead — open-source development has
 *   demonstrated for decades that software is produced at quality and scale
 *   without proprietary restrictions — yet the constraint persists because
 *   vendors extract monopoly rents from artificial scarcity. The reading does
 *   NOT claim proprietary software is a mountain or a genuine natural law; it
 *   claims it is a constructed extractive system maintained by concentrated
 *   institutional power.
 *
 * KEY AGENTS:
 *   - Proprietary software vendors: institutional power, agenda-setter, collects rents through licensing restrictions, enforces via copyright and contract law
 *   - Software users (globally, all skill levels): powerless, identity-locked (digital infrastructure mandatory), cannot inspect or modify code they depend on
 *   - Developers restricted from modification: moderate power, constrained exit, cannot legally study or improve foundational software
 *   - Open-source development communities: organized, demonstrate viability of freedom-respecting model
 *   - Intellectual property law and corporate profit apparatus: non-agent, structural substrate that sustains the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.92).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.88).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software as Imposed Injustice (Freedom-Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, 'ed85d8d6-58de-40e4-89a4-69d27e01b014').
narrative_ontology:cs_kernel_codification('ed85d8d6-58de-40e4-89a4-69d27e01b014', distributed).
narrative_ontology:cs_authority_grounding('ed85d8d6-58de-40e4-89a4-69d27e01b014', extraction).
narrative_ontology:cs_interpretation_layer_present('ed85d8d6-58de-40e4-89a4-69d27e01b014').
narrative_ontology:cs_reading_relation('ed85d8d6-58de-40e4-89a4-69d27e01b014', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('ed85d8d6-58de-40e4-89a4-69d27e01b014', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed85d8d6-58de-40e4-89a4-69d27e01b014', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('ed85d8d6-58de-40e4-89a4-69d27e01b014', foundational, software_freedom_inalienable_right).
narrative_ontology:cs_axiom_status(software_freedom_inalienable_right, holdable).
narrative_ontology:cs_axiom_grounding('ed85d8d6-58de-40e4-89a4-69d27e01b014', software_freedom_inalienable_right, deontological).
narrative_ontology:cs_axiom('ed85d8d6-58de-40e4-89a4-69d27e01b014', foundational, source_access_non_negotiable).
narrative_ontology:cs_axiom_status(source_access_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('ed85d8d6-58de-40e4-89a4-69d27e01b014', source_access_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('ed85d8d6-58de-40e4-89a4-69d27e01b014', universal_software_commons).
narrative_ontology:cs_drift_state('ed85d8d6-58de-40e4-89a4-69d27e01b014', contemporary_proprietary_deepening, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed85d8d6-58de-40e4-89a4-69d27e01b014', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, developers_restricted_from_modification).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, downstream_innovation_ecosystem).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, open_source_developers).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, software_freedom_as_fundamental_right).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, commons_superiority_for_digital_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set licensing terms that restrict source code access, modification, and distribution. Justify restrictions as protecting investment, ensuring quality control, and maintaining business model viability. Actively enforce via legal instruments (copyright, patent, DMCA), technical measures (obfuscation, DRM), and contractual terms. Collect rents from licensing, support, and vendor lock-in.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Cannot inspect the code they run, cannot modify it to fix bugs or remove surveillance, cannot redistribute it to others. Dependent on vendor decisions for security patches, feature development, and software lifetime. Digital existence is increasingly mandatory (work, finance, health, civic participation), making exit a form of social exclusion. Their powerlessness is structural: no individual user has negotiating leverage; the license is a take-it-or-be-excluded offer.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_users, payer,
    powerless, biographical, identity_locked, global).

% Cannot legally study, modify, or redistribute the software they depend on or build atop. Restricted to the vendor's development roadmap; cannot fix bugs that affect their use cases or remove features that conflict with their values. Professional and ethical autonomy is constrained by licensing. Can theoretically exit to open-source alternatives, but switching costs are high and coverage is often incomplete (no open-source equivalent exists for critical domains).
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, developers_restricted_from_modification, payer,
    moderate, biographical, constrained, global).

% Innovation is slowed by inability to build on proprietary software, by reinvention of solved problems, by lock-in that fragments development effort. Communities that would otherwise contribute to shared infrastructure cannot because the primary implementations are proprietary. Collective human knowledge and capability are artificially restricted at the point where they should accumulate most freely.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, downstream_innovation_ecosystem, payer,
    organized, generational, constrained, global).

% Build and maintain software under freedom-respecting licenses, making source available and modification possible. Operate outside the proprietary constraint and demonstrate the viability of the alternative model. Often viewed as the analytical seat that tests the freedom-imperative claim empirically — they show that software development works without source-code restriction.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, open_source_developers, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, open_source_developers, observer).

% The institutional apparatus that vindicates and enforces proprietary software licensing: venture capital, intellectual property law, patent offices, the DMCA, contract enforcement machinery. Not an actor itself, but the structure that sustains the constraint's operation and prevents its dissolution even when alternatives exist.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, corporate_profit_capture_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(software_source_status__freedom_imperative_reading, corporate_profit_capture_apparatus).

% Societies cannot audit or control their own digital infrastructure because it rests on proprietary software they cannot inspect or modify. National sovereignty and collective self-determination are compromised when critical infrastructure is controlled by foreign vendors answerable only to shareholders. Would benefit from open-source infrastructure but are excluded from the decision-making that shapes current software stacks.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, societies_dependent_on_digital_infrastructure, excluded,
    organized, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Proprietary software licensing appears to coordinate developer incentives and quality assurance: vendors invest in development, users get finished products with vendor accountability. The freedom-imperative reading denies this is genuine coordination — it is extraction disguised as governance.
% TRANSFER_FUNCTION: Transfers control, autonomy, and knowledge from software users and developers to proprietary vendors. Users surrender the ability to audit, modify, and distribute code; vendors extract monopoly rents on the output and maintain permanent control over its evolution. Developers give up the right to study and improve the tools they use.
% ABSENT_VOICES: The voices absent are those silenced by the constraint itself: the user who discovers a security vulnerability but cannot fix it; the developer in the Global South who cannot afford the licensing fees for tools that are otherwise free elsewhere; the society that would choose open infrastructure but finds it already locked into proprietary stacks; the future generations who would inherit software they cannot understand or modify.
% DISAPPEARANCE_RATIONALE: If proprietary software licensing disappeared overnight, the software stack would reorganize around open-source implementations. Some vendors would collapse; others would shift to service-based models. Users would gain the ability to audit, modify, and redistribute. The ecosystem would experience transient pain as proprietary systems were replaced, but the underlying capability to develop and maintain software would not disappear — it would decentralize and spread. The constraint exists not because software cannot be made without it, but because vendors extract more by restricting it than by selling it openly.
% FOUNDING_PROBLEM: Early computing lacked business models for software development; vendors needed to recoup R&D and ensure revenue streams. Proprietary licensing and copyright restriction were adopted as a mechanism to create artificial scarcity and extract rents from software's ability to be freely reproduced.
% FOUNDING_PROBLEM_CORROBORATION: Open-source development has demonstrated for 30+ years that software is produced at scale, at quality, and at innovation speed without proprietary licensing restrictions. Linux, Apache, Firefox, Kubernetes, the entire web infrastructure — all prove the founding problem is solved. The constraint persists not because the problem is live but because vendors benefit from continuing it. Software companies themselves (Amazon, Google, Microsoft) use open-source internally while selling proprietary offerings externally, proving they understand the model's viability but choose extraction over contribution.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.92) reflects the reading's axiom: proprietary software, under this reading, extracts nearly total control from users — they surrender autonomy, transparency, and modification rights in exchange for access to functionality. The suppression score (0.88) reflects high enforcement intensity: copyright law, DMCA, contractual terms, and technical DRM all actively prevent users and developers from exercising freedom. Theater ratio (0.42) is moderate because the constraint operates on a mix of genuine governance (quality assurance, security review) and pure extraction (lock-in, rent collection); as the interval progresses, theater rises because companies adopt open-source components while maintaining proprietary wrapping, performing 'openness' while preserving control. Accessibility collapse (0.79) reflects that alternatives do exist (open source), so the collapse is substantial but not complete — identity lock and economic dependence on proprietary ecosystems reduce realistic exit. Resistance (0.71) reflects significant push-back from open-source communities and policy advocates, but institutional power of vendors maintains the constraint. The measurements show steady extraction increase over the 40-unit interval: base extractiveness rises from 0.78 to 0.92 as cloud computing and SaaS deepen vendor control (software-as-a-service eliminates even the option to run local modified versions); suppression requirement rises from 0.75 to 0.88 as anti-modification tooling (DRM, cloud-only architecture) intensifies; theater rises from 0.28 to 0.42 as vendors adopt ceremonial open-source commitments while their core products remain proprietary.
 *
 * PERSPECTIVAL GAP:
 *   The constraint should compute differently from vendor vs. user seats. From the vendor's institutional position, the constraint is genuine coordination: they fund development, maintain code quality, provide support, and deserve compensation via licensing revenue — the structure looks like rope or even natural market equilibrium. From the user's powerless, identity-locked position, the same structure is pure extraction: mandatory dependency on proprietary tools, inability to audit or modify, control held indefinitely by a vendor answerable only to shareholders. The engine computes this divergence from the structural data: vendors have arbitrage exit (they can shift to alternative profit models, have leverage to negotiate), high power (institutional), and are beneficiaries; users have identity-locked exit (digital infrastructure is mandatory), powerless status, and are payers. The divergence is structural, not perceptual — the constraint genuinely operates as coordination-from-above and extraction-from-below simultaneously. The claimed_type (snare) reflects the freedom-imperative reading's interpretation: the coordination function is cover; the real operation is extraction masked as governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: proprietary_software_vendors (d = 0.0, full beneficiary — they collect the rents, control the rules, have exit options). Payers: software_users (d = 0.95, near-full target — powerless, identity-locked, trapped in mandatory digital infrastructure, pay through licensing fees and vendor lock-in, cannot exit without social exclusion), developers_restricted_from_modification (d = 0.8, high target — constrained exit, moderate power, pay through inability to study/improve/modify), downstream_innovation_ecosystem (d = 0.75, substantial target — organized power mitigates slightly, but constrained by inability to build on proprietary foundations). No directionality overrides are needed; the canonical derivation from beneficiary/victim + power + exit produces appropriate d values. The asymmetry is extreme because the constraint is designed to concentrate gains (vendors) and distribute costs (users across the globe, each individually powerless).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of business models for software development) is classified as DEAD: open-source software has demonstrated at scale that development is viable without proprietary licensing — Linux, Apache, the entire modern web stack, all prove the original problem is solved. Yet the constraint persists. This is the mandatrophy signature: the arrangement was built to solve a problem that no longer exists, and it persists because those who benefit from it (vendors) have the power to maintain it, while those who would dissolve it (users) do not. The founding_problem_status (dead) + disappearance_verdict (world_rearranges) mismatch signals mandatrophy: if the constraint disappeared, the software ecosystem would not collapse (alternatives exist), but vendors would lose substantial revenue. The constraint is not held in place by necessity; it is held by force. This classification prevents misreading proprietary software as a natural law or genuine coordination — it identifies it as a constructed extraction mechanism justified by a founding problem that is no longer live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_vs_pragmatic_empirical_boundary,
    'Is the claim that software freedom is a fundamental ethical right grounded in deontological principle (inalienable, prior to utility), or is it instrumentally grounded in empirical claims about innovation and quality outcomes?',
    'Decompose the freedom-imperative reading into its foundational axioms: if the core claim survives disproof of the empirical outcomes (code quality, innovation speed, security), the grounding is deontological; if the reading collapses when empirical outcomes favor proprietary models, the grounding is instrumental and the reading collapses into the pragmatic reading.',
    'If deontological, the reading is foreclosed only by logical contradiction (another axiom denying freedom as inalienable), not by evidence of pragmatic inferiority. If instrumental, the pragmatic_development_reading could supersede this one if empirical data shifts — the reading''s survival depends on outcome evidence, not principle. This determines whether the reading can be refuted or only disagreed with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_vs_pragmatic_empirical_boundary, conceptual, 'Whether the freedom-imperative grounding is deontological or empirically contingent.').

omega_variable(
    inalienable_rights_implementation_gap,
    'If software freedom is an inalienable right, who or what enforces it, and how is it distinguished from other claims to inalienable rights that remain unforced?',
    'Map the enforcement pathway: does the reading rely on legal reform (new legislation mandating open source), market pressure (consumers choosing open source), or a philosophical claim that cannot be enforced (a ''right'' that exists regardless of enforcement, like dignity under oppression)? If the first, the reading enters a contest with property-rights reading over legitimate legislative authority. If the second, the reading predicts market dynamics that can be tested empirically. If the third, clarify the reading''s actual claim boundary.',
    'If enforcement-dependent, the reading becomes a normative position competing with others for political power — not a claim about what IS but what SHOULD be. If market-dependent, the reading can be falsified by market outcomes. If philosophical, the reading makes a claim immune to verification but also less actionable, shifting the constraint into the conceptual rather than the structural domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inalienable_rights_implementation_gap, preference, 'How the claimed inalienable right to software freedom would be enforced or distinguished from unenforced claims.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.88) structural (users cannot exit because of technical, legal, and economic barriers) or internalized (users have accepted proprietary software as legitimate and would not exit even if barriers dissolved)?',
    'Post-barrier dissolution test: if proprietary software licensing were made illegal and users were given costless access to equivalent open-source alternatives, how many would switch? If most remained on proprietary software, suppression is substantially internalized. If most switched, suppression is structural. Partial switching indicates mixed mechanism.',
    'If internalization is high, the constraint operates through captured values and normalized practices — users believe in property rights for code even though the reading claims they should not. The reading''s claim that proprietary software is an injustice would then meet internalized acceptance of the injustice itself, requiring cultural re-education, not just legal reform. If structural, legal and market reforms would be sufficient to dissolve the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether measured suppression is structural or internalized in user practices and beliefs.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the software_source_status kernel. How does the freedom-imperative reading''s axioms and reference frame differ from the property_rights_reading and pragmatic_development_reading?',
    'Comparative structural analysis: the freedom-imperative reading grounds legitimacy in inalienable user rights and the deontological claim that source access is non-negotiable; the property_rights_reading grounds legitimacy in creator authority and incentives; the pragmatic_development_reading grounds legitimacy in empirical software quality outcomes. These are not mere disagreements about the same fact — they are incommensurable reference frames over what makes a licensing regime legitimate.',
    'The reading_relations and axioms fields in cs_structure record this structural distinction: freedom-imperative forecloses property_rights (their foundational axioms directly contradict), coexists_with pragmatic_development (both could be held by different parties), and influences utilitarian_hybrid (by establishing freedom as a value that utilities must trade off against). Understanding the committer structure prevents misclassifying disagreement as empirical when it is foundational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'The committer-frame structure of this reading relative to its siblings in the software_source_status kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_source_status__freedom_imperative_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_source_status__freedom_imperative_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_source_status__freedom_imperative_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t25, software_source_status__freedom_imperative_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(soft_tr_t25, observed).
narrative_ontology:measurement(soft_tr_t35, software_source_status__freedom_imperative_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(soft_tr_t35, observed).
narrative_ontology:measurement(soft_tr_t40, software_source_status__freedom_imperative_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(soft_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_source_status__freedom_imperative_reading, base_extractiveness, 5, 0.82).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_source_status__freedom_imperative_reading, base_extractiveness, 10, 0.85).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_source_status__freedom_imperative_reading, base_extractiveness, 15, 0.88).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t25, software_source_status__freedom_imperative_reading, base_extractiveness, 25, 0.91).
narrative_ontology:measurement_basis(soft_be_t25, observed).
narrative_ontology:measurement(soft_be_t35, software_source_status__freedom_imperative_reading, base_extractiveness, 35, 0.92).
narrative_ontology:measurement_basis(soft_be_t35, observed).
narrative_ontology:measurement(soft_be_t40, software_source_status__freedom_imperative_reading, base_extractiveness, 40, 0.92).
narrative_ontology:measurement_basis(soft_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_source_status__freedom_imperative_reading, suppression_requirement, 5, 0.79).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_source_status__freedom_imperative_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_source_status__freedom_imperative_reading, suppression_requirement, 15, 0.84).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t25, software_source_status__freedom_imperative_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement_basis(soft_su_t25, observed).
narrative_ontology:measurement(soft_su_t35, software_source_status__freedom_imperative_reading, suppression_requirement, 35, 0.88).
narrative_ontology:measurement_basis(soft_su_t35, observed).
narrative_ontology:measurement(soft_su_t40, software_source_status__freedom_imperative_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement_basis(soft_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(software_source_status__freedom_imperative_reading, 0.08).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% The software_source_status kernel decomposes into four constraint stories, one per reading. Each reading instantiates a different constraint because the ε-referent (the standing proprietary arrangement under contest) is assessed differently by each reading: freedom-imperative reads it as nearly total extraction (ε=0.92), property_rights reads it as legitimate coordination (ε~0.3), pragmatic reads it as suboptimal methodology (ε~0.5), utilitarian reads it as context-dependent (ε ranges). These are not the same constraint viewed from different angles — they are structurally distinct constraints because each reading carries an incommensurable reference frame about legitimacy. They are linked via network.affects_constraints because each reading's certification affects the others' credibility and the empirical evidence that would resolve their disagreement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
