% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text (Corporate Moat Reading)
 *   domain: intellectual_property/software_licensing/technology_governance
 *
 * SUMMARY:
 *   Permissive open source licenses (MIT, Apache 2.0, BSD) allow corporations
 *   to incorporate code into proprietary products, modify it, and resell
 *   without reciprocal obligation. This reading frames the arrangement as a
 *   snare: the legal structure enables uncompensated extraction of maintainer
 *   labor for corporate profit, sustained by cultural mythology ('permissive
 *   = maximum freedom') and path-dependent adoption lock-in. The founding
 *   problem (complex licensing as adoption barrier) is solved, but the
 *   mechanism persists as institutional inertia layered with active corporate
 *   defense of the permissive narrative. This is ONE reading of the
 *   permissive-license kernel; the commons-coordination and
 *   copyleft-counterfactual readings are structurally distinct constraints
 *   with different ε and victim sets (separate JSON files, linked via
 *   network.affects_constraints). This reading's claim (snare) and metrics
 *   (high extractiveness, high suppression, moderate theater) align
 *   intentionally: the constraint operates as extraction sustained by
 *   enforced narrative control.
 *
 * KEY AGENTS:
 *   - enterprise_corporations: institutional power, arbitrage exit — capture uncompensated maintenance labor via permissive license terms
 *   - individual_maintainers: powerless, identity-locked exit — bear ongoing maintenance cost, cannot demand reciprocal contribution without violating the license choice they made
 *   - small_open_source_communities: moderate power, constrained exit — collectively maintain libraries but face resource scarcity and are culturally pressured to stay permissive
 *   - licensing_standards_bodies: organized observer role — define the permissive/copyleft framing and validate permissive as 'maximum freedom', obscuring the extraction logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.68).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.72).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text (Corporate Moat Reading)").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "intellectual_property/software_licensing/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '63ec494c-3eaf-490c-9f68-05c44ca8d13b').
narrative_ontology:cs_kernel_codification('63ec494c-3eaf-490c-9f68-05c44ca8d13b', formalized).
narrative_ontology:cs_authority_grounding('63ec494c-3eaf-490c-9f68-05c44ca8d13b', extraction).
narrative_ontology:cs_interpretation_layer_present('63ec494c-3eaf-490c-9f68-05c44ca8d13b').
narrative_ontology:cs_reading_relation('63ec494c-3eaf-490c-9f68-05c44ca8d13b', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('63ec494c-3eaf-490c-9f68-05c44ca8d13b', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('63ec494c-3eaf-490c-9f68-05c44ca8d13b', foundational, property_rights_maximize_market_value).
narrative_ontology:cs_axiom_status(property_rights_maximize_market_value, holdable).
narrative_ontology:cs_axiom_grounding('63ec494c-3eaf-490c-9f68-05c44ca8d13b', property_rights_maximize_market_value, empirically_contingent).
narrative_ontology:cs_axiom('63ec494c-3eaf-490c-9f68-05c44ca8d13b', foundational, permissive_license_enables_corporate_moat).
narrative_ontology:cs_axiom_status(permissive_license_enables_corporate_moat, holdable).
narrative_ontology:cs_axiom_grounding('63ec494c-3eaf-490c-9f68-05c44ca8d13b', permissive_license_enables_corporate_moat, empirically_contingent).
narrative_ontology:cs_reference_frame('63ec494c-3eaf-490c-9f68-05c44ca8d13b', permissive_license_as_adoption_accelerant).
narrative_ontology:cs_drift_state('63ec494c-3eaf-490c-9f68-05c44ca8d13b', contemporary_post_adoption_saturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('63ec494c-3eaf-490c-9f68-05c44ca8d13b', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, small_open_source_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, proprietary_software_companies).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, end_users).
narrative_ontology:constraint_vindicates(permissive_license_text__corporate_moat_reading, property_rights_drive_innovation).
narrative_ontology:constraint_vindicates(permissive_license_text__corporate_moat_reading, open_source_sustainability_through_corporate_adoption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Selects and adopts permissive-license open source software (Apache 2.0, MIT, BSD). Integrates the code into proprietary products, modifies it for competitive advantage, and sells the resulting proprietary packages without legal obligation to share modifications or contribute revenue back to original maintainers. Controls the narrative that permissive licensing 'maximizes freedom' and adoption velocity. Benefits from uncompensated labor of maintainers, free R&D that competitors cannot easily replicate, and legal shield against modification demands.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_corporations, agenda_setter,
    institutional, generational, arbitrage, global).

% Creates and maintains high-quality open source software under a permissive license, often as unpaid labor driven by reputation, professional identity, or altruistic commitment to the commons. Bears the ongoing cost of maintenance, security updates, and community support. Watches as corporations incorporate their work into proprietary products, extract value, and reinvest zero back to the original project. Cannot exit without abandoning professional identity and the open source community that constitutes their epistemic peer group; cannot demand reciprocity within the terms they have chosen.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, individual_maintainers, beneficiary).

% Collectively maintain libraries, frameworks, and tools under permissive licenses that corporations depend on. Experience resource scarcity (funding, developer time) while watching enterprise consumers build for-profit services on top without meaningful contribution back. Can attempt to switch to copyleft (GPL) but face adoption penalties from corporate ecosystem lock-in and may fracture their own community over license choice.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, small_open_source_communities, payer,
    moderate, biographical, constrained, global).

% Compete on integration and packaging of permissive-licensed components into proprietary systems. Benefit from uncompensated improvement and testing from the distributed open source community while legally avoiding reciprocal contribution. Their proprietary moat is reinforced by their ability to combine and modify open components in ways the original maintainers cannot directly access.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, proprietary_software_companies, beneficiary,
    powerful, generational, mobile, global).

% Gain access to proprietary products at lower cost and higher quality because corporations have incorporated high-quality open source components without bearing R&D cost. Benefit from innovation that permissive licensing enables. Also depend on the continued goodwill and volunteer work of maintainers; if maintainers burn out, the entire dependency chain degrades.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, end_users, beneficiary,
    organized, biographical, constrained, global).

% Wish to create closed-source proprietary products that depend on permissive-licensed open source (thus competing with the corporations that have already captured the moat). Are blocked by the same uncompensated extraction that harms individual maintainers: they cannot afford to build proprietary improvements on top without corporate-scale resources, and the legal landscape (permissive license + corporate adoption path dependency) creates a structure where only well-capitalized actors can extract value at scale.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, competing_maintainers, excluded,
    powerless, biographical, trapped, global).

% Define and promote permissive licensing frameworks (OSI, SPDX, etc.) as 'maximizing freedom.' Their institutional mission is to simplify and canonicalize licensing; the permissive/copyleft divide sits at the center of their definitional authority. They face pressure from corporate adopters to validate permissive licenses and from maintainer advocates to recognize the asymmetry the corporate reading instantiates.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, licensing_standards_bodies, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing reduces legal friction for adoption and modification of open source code, enabling rapid distribution of high-quality software components across organizational boundaries without negotiated licensing overhead.
% TRANSFER_FUNCTION: Transfers uncompensated labor (maintenance, debugging, innovation) from individual and small-community maintainers to corporations that incorporate the code into proprietary products, extract market value, and reinvest minimally back to the source.
% ABSENT_VOICES: Maintainers who chose permissive licensing to maximize adoption (not understanding the corporate moat dynamic at the time) would object if given ex-post choice. Developers who wanted to build proprietary derivatives on permissive foundations but lacked capital to do so are excluded by the path dependency the early corporate adopters created. Academic and social-benefit sectors that depend on permissive-licensed code are silenced by the corporate narrative that 'permissive = freedom'.
% DISAPPEARANCE_RATIONALE: If the permissive-license structure and its corporate enforcement vanished, maintainers would shift to copyleft licensing or closed-source models; corporations would either contribute back to projects or fund internal alternatives; the ecosystem would reorganize around reciprocal obligation. The current equilibrium — where uncompensated extraction is legally permitted and culturally normalized — would dissolve.
% FOUNDING_PROBLEM: Early open source (1990s) faced fragmentation and licensing complexity that slowed adoption by corporations and communities. Permissive licensing was designed to minimize legal friction and accelerate the distribution of high-quality code to maximize global implementation.
% FOUNDING_PROBLEM_CORROBORATION: Corporate ecosystem testimonies from OSI and Linux Foundation align with the founding rationale. However, maintainer testimony (collected in annual surveys by GitHub, Tidelift, and independent researcher interviews since 2018) and academic analysis (Neville-Neil, 2021; Eghbal, 2020) from outside the corporate ecosystem corroborate that the founding problem (licensing complexity as a barrier to adoption) has been solved for at least a decade, while the extraction mechanism persists and has intensified.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 because the legal structure (permissive license) is decoupled from the actual cost of maintenance: corporations gain competitive advantages (ability to hide modifications, no reciprocal R&D obligation) worth billions annually from code they do not pay for. Suppression is high (0.72) because the constraint's persistence depends on active narrative control: corporate participants and standard-setting bodies continuously assert that permissive licensing 'maximizes freedom', a framing that obscures the asymmetric extraction and discourages maintainers from switching to reciprocal licensing (which would break corporate moat advantages). Theater is moderate (0.41) and rising slowly: the narrative claim ('we chose permissive because we care about freedom') serves as functional cover for extraction, but the claim is partially sincere — permissive licensing DID maximize early adoption. The measurement series shows extractiveness plateauing around t=25 as the ecosystem matures and the founding-problem justification fully evaporates; suppression remains stable as corporate enforcement of the permissive narrative holds steady; theater rises as the historical rationale ('adoption barrier removal') becomes less credible and the narrative must work harder to suppress the extraction reading.
 *
 * PERSPECTIVAL GAP:
 *   The corporate agenda-setter seat experiences this as genuine coordination and opportunity: they read the permissive license as permission to improve and combine components, a freedom they legitimately won. The maintainer seat experiences the same legal structure as entrapment: they chose permissive intending to help, only to watch their labor become the unpaid R&D substrate for corporate products they have no access to and no claim on. The licensing-standards-body observer seat is structurally committed to the corporate reading (their institutional role is to validate and simplify open standards, which corporates drive). From the maintainer's seat, the type divergence is maximal: corporate actor sees rope (genuine coordination), maintainer sees snare (enforced extraction). The engine computes this per-seat; the authored metrics and beneficiary/victim structure make the divergence transparent.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise corporations are the structural beneficiaries (d ≈ 0.15): they benefit from uncompensated extraction without bearing the maintenance cost; their exit options are mobile (they can fork, rewrite, or adopt competing projects) and they have institutional power to shape the ecosystem. Individual maintainers are the targets (d ≈ 0.85): they bear the ongoing cost of maintenance, cannot exit without abandoning their professional identity and peer community, and are trapped in a legal structure that permits corporations to profit from their work without reciprocal obligation. Small communities sit between (d ≈ 0.55): they gain adoption reach and reputation from corporate use, but the constraint extracts value from their collective labor. The directionality override is not needed because the derivation from beneficiary/victim + exit + power correctly produces the asymmetry: corporate institutional power + arbitrage exit → low d; maintainer powerlessness + identity lock → high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy: the founding mandate was to reduce licensing complexity as a barrier to adoption. That mandate was accomplished by ~2010; permissive licensing is now the ecosystem standard and adoption is trivial. However, the constraint (the legal and cultural framework that enforces permissive licensing on maintainers) persists because corporate beneficiaries capture the enforcement infrastructure. Maintainers cannot easily switch to copyleft without accepting ecosystem fragmentation and adoption penalties. Standards bodies have institutional lock-in to the permissive-=freedom narrative. The mandate is dead (adoption barriers are solved), but the mechanism persists as institutional inertia defended by those who profit. The theater_ratio rise (from 0.25 to 0.41) reflects the increasing performative work required to maintain the 'freedom' narrative as the contradiction becomes more visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maintained_vs_abandoned_projects,
    'Does the extraction mechanism operate identically on actively-maintained high-value projects versus abandoned or dormant projects?',
    'Empirical survey of corporate adoption patterns: do corporations preferentially extract from projects with active maintainers (ongoing R&D subsidies) versus genuinely abandoned code (where extraction has zero victim)? Do maintainers experience higher pressure when their project is valuable?',
    'If the extraction mechanism tracks maintainer activity (corporations benefit MORE from maintained projects), the snare classification is strengthened. If extraction is indifferent to maintenance status, the dynamic is closer to scavenging on abandoned property — a different type classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maintained_vs_abandoned_projects, empirical, 'Whether the constraint systematically extracts more from actively-maintained projects.').

omega_variable(
    identity_lock_sustainability,
    'Is the identity_lock exit option for maintainers structural (genuinely unable to exit without ceasing to be a software developer) or internalized (maintainers believe they cannot exit because of professional norms and self-concept)?',
    'Longitudinal study of maintainers who switched to closed-source or copyleft licensing: did their professional identity persist? Do communities reorganize after a high-value project switches licenses? Track whether the suppression that keeps maintainers ''locked'' persists after they exit the permissive ecosystem.',
    'If internalized: the effective suppression is higher than the authored value — maintainers carry the lock with them. If structural: the lock is a real feature of the professional ecosystem and reflects genuine constraints on exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_sustainability, empirical, 'Identity-lock mechanism classification (structural vs. internalized).').

omega_variable(
    founding_problem_genuinely_dead,
    'Is the original founding problem (licensing complexity as an adoption barrier) genuinely solved, or does corporate complexity around licensing still exist (hidden in ''enterprise licensing'' practices)?',
    'Analysis of corporate licensing practices post-2015: do corporations face meaningful licensing friction when adopting permissive code? Have adoption rates continued to accelerate post-2010, or have they plateaued? Interview enterprise architects about licensing decision time.',
    'If the problem is truly dead and the constraint persists purely through extraction, the mandatrophy verdict is firm. If the problem is merely shifted (simpler legal text, but harder practical integration work), the constraint retains partial coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_genuinely_dead, empirical, 'Whether the founding problem (licensing friction) is dead or persists in different form.').

omega_variable(
    kernel_reading_distinction,
    'Are the corporate_moat_reading and commons_coordination_reading genuinely distinct constraints, or are they the same constraint read from different observer positions?',
    'Structural test: the commons_coordination_reading would author beneficiaries as ''global_developer_community'' and victims as ''none'', with high accessibility_collapse (alternatives are legally equivalent) and low resistance (no one opposes permissive licensing on principle). This reading authors beneficiaries as ''enterprise_corporations'', victims as ''individual_maintainers'', and moderate accessibility_collapse (alternatives like copyleft exist but are suppressed). If the beneficiary/victim structure diverges, the readings instantiate different ε values and are indeed distinct constraints.',
    'If distinct: the kernel is genuinely contested and the three readings are three separate constraint stories (three JSON files, linked via network). If not distinct: the kernel framing is imposed and the reading is really an observer-choice overlay on a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether permissive-license kernel readings are structurally distinct constraints or observer-relative frames.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pltcmr_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(pltcmr_tr_t0, observed).
narrative_ontology:measurement(pltcmr_tr_t5, permissive_license_text__corporate_moat_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(pltcmr_tr_t5, observed).
narrative_ontology:measurement(pltcmr_tr_t10, permissive_license_text__corporate_moat_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(pltcmr_tr_t10, observed).
narrative_ontology:measurement(pltcmr_tr_t15, permissive_license_text__corporate_moat_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(pltcmr_tr_t15, observed).
narrative_ontology:measurement(pltcmr_tr_t20, permissive_license_text__corporate_moat_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(pltcmr_tr_t20, observed).
narrative_ontology:measurement(pltcmr_tr_t25, permissive_license_text__corporate_moat_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(pltcmr_tr_t25, observed).
narrative_ontology:measurement(pltcmr_tr_t30, permissive_license_text__corporate_moat_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(pltcmr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(pltcmr_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(pltcmr_be_t0, observed).
narrative_ontology:measurement(pltcmr_be_t5, permissive_license_text__corporate_moat_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(pltcmr_be_t5, observed).
narrative_ontology:measurement(pltcmr_be_t10, permissive_license_text__corporate_moat_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(pltcmr_be_t10, observed).
narrative_ontology:measurement(pltcmr_be_t15, permissive_license_text__corporate_moat_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(pltcmr_be_t15, observed).
narrative_ontology:measurement(pltcmr_be_t20, permissive_license_text__corporate_moat_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(pltcmr_be_t20, observed).
narrative_ontology:measurement(pltcmr_be_t25, permissive_license_text__corporate_moat_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(pltcmr_be_t25, observed).
narrative_ontology:measurement(pltcmr_be_t30, permissive_license_text__corporate_moat_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(pltcmr_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(pltcmr_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(pltcmr_su_t0, observed).
narrative_ontology:measurement(pltcmr_su_t5, permissive_license_text__corporate_moat_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(pltcmr_su_t5, observed).
narrative_ontology:measurement(pltcmr_su_t10, permissive_license_text__corporate_moat_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(pltcmr_su_t10, observed).
narrative_ontology:measurement(pltcmr_su_t15, permissive_license_text__corporate_moat_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(pltcmr_su_t15, observed).
narrative_ontology:measurement(pltcmr_su_t20, permissive_license_text__corporate_moat_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(pltcmr_su_t20, observed).
narrative_ontology:measurement(pltcmr_su_t25, permissive_license_text__corporate_moat_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(pltcmr_su_t25, observed).
narrative_ontology:measurement(pltcmr_su_t30, permissive_license_text__corporate_moat_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(pltcmr_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__corporate_moat_reading, 0.18).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the permissive_license_text kernel. The commons_coordination_reading authorizes the same text as a rope (genuine coordination maximizing freedom); the copyleft_counterfactual_reading evaluates the counterfactual GPL alternative as a superior snare-preventive structure. All three readings share the artifact (permissive-license text) but instantiate different constraints (different ε, different beneficiary/victim structures, different type classifications). The corporate_moat_reading frames permissive licensing as enabling uncompensated extraction; the commons_coordination_reading frames it as minimizing friction; the copyleft_counterfactual examines what reciprocity would prevent. Each reading is authored as a closed constraint with its own metrics; the network links document the family relationship and decomposition rationale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
