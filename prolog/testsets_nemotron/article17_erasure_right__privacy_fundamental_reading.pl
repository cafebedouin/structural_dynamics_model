% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__privacy_fundamental_reading, []).

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
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: Article 17 Right to Erasure — Privacy Fundamental Reading
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   Article 17 GDPR (Right to Erasure) instantiates individual data
 *   sovereignty as a fundamental right limiting corporate data retention.
 *   This reading treats the erasure right as a genuine coordination
 *   mechanism: it solves the collective-action problem of asymmetrical data
 *   power by giving individuals a legally enforceable lever over data
 *   controllers. The constraint coordinates the relationship between data
 *   subjects and processors by establishing a baseline — data must be erased
 *   when its purpose is fulfilled, consent is withdrawn, or processing is
 *   unlawful — backed by regulatory enforcement. Platforms and data
 *   controllers are the constrained parties; they bear compliance costs but
 *   the arrangement's primary function is protecting the beneficiary class
 *   (data subjects) from permanent data captivity. The claimed type is rope:
 *   a coordination function with minimal coercive overhead, net beneficiary
 *   participation, and no suppression of alternatives (subjects can still
 *   consent to retention; controllers can still process with lawful basis).
 *   Extraction is low (0.32) and primarily reflects compliance overhead, not
 *   rent extraction. Suppression is low (0.18) — the constraint suppresses
 *   only unlawful retention, not lawful speech or competition. Theater is
 *   moderate (0.22) — some performative compliance exists (automated
 *   rejection templates, overly narrow interpretations) but core function is
 *   operational.
 *
 * KEY AGENTS:
 *   - data_subjects_general: Primary beneficiary (organized/constrained) — gains enforceable control over personal data lifecycle
 *   - vulnerable_data_subjects: Intensified beneficiary (powerless/trapped) — children, victims, marginalized groups for whom erasure is existentially significant
 *   - digital_rights_advocates: Beneficiary/observer (organized/analytical) — leverage the right for structural challenges and precedent-setting
 *   - large_platforms: Constrained party/agenda_setter (institutional/arbitrage) — bear compliance infrastructure costs; shape implementation through technical design
 *   - smb_data_controllers: Constrained party/payer (moderate/constrained) — bear disproportionate per-unit compliance costs; limited technical capacity
 *   - regulatory_authorities: Agenda_setter/observer (institutional/analytical) — enforce, interpret, and evolve the right through guidelines and decisions
 *   - competing_platforms_entrants: Excluded/payer (moderate/trapped) — face entry barriers from compliance infrastructure requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.32).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.18).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "Article 17 Right to Erasure — Privacy Fundamental Reading").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, '8c7f9933-633d-4183-902e-0de3a55bb025').
narrative_ontology:cs_kernel_codification('8c7f9933-633d-4183-902e-0de3a55bb025', formalized).
narrative_ontology:cs_authority_grounding('8c7f9933-633d-4183-902e-0de3a55bb025', lineage).
narrative_ontology:cs_interpretation_layer_present('8c7f9933-633d-4183-902e-0de3a55bb025').
narrative_ontology:cs_reading_relation('8c7f9933-633d-4183-902e-0de3a55bb025', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c7f9933-633d-4183-902e-0de3a55bb025', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('8c7f9933-633d-4183-902e-0de3a55bb025', foundational, personal_data_sovereignty_fundamental).
narrative_ontology:cs_axiom_status(personal_data_sovereignty_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('8c7f9933-633d-4183-902e-0de3a55bb025', personal_data_sovereignty_fundamental, deontological).
narrative_ontology:cs_axiom('8c7f9933-633d-4183-902e-0de3a55bb025', foundational, erasure_as_restoration_not_exception).
narrative_ontology:cs_axiom_status(erasure_as_restoration_not_exception, holdable).
narrative_ontology:cs_axiom_grounding('8c7f9933-633d-4183-902e-0de3a55bb025', erasure_as_restoration_not_exception, deontological).
narrative_ontology:cs_reference_frame('8c7f9933-633d-4183-902e-0de3a55bb025', gdpr_article17_textual_obligation).
narrative_ontology:cs_drift_state('8c7f9933-633d-4183-902e-0de3a55bb025', post_schrems_ii_guidance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c7f9933-633d-4183-902e-0de3a55bb025', '2026-08-03T14:22:10Z').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_subjects_general).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, vulnerable_data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, digital_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, large_platforms).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, smb_data_controllers).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, competing_platforms_entrants).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, data_sovereignty_fundamental_right).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, informational_self_determination).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, purpose_limitation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary individuals whose personal data is processed by platforms, services, and organizations. They gain a legally enforceable right to request erasure when data is no longer necessary, consent is withdrawn, or processing is unlawful. Exercising the right requires submitting a request (often via web form) and waiting for compliance. They cannot easily exit the data ecosystem — modern life requires digital services — so the right's legal enforceability is essential. They benefit from the coordination without bearing infrastructure costs.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_subjects_general, beneficiary,
    organized, biographical, constrained, continental).

% Children, victims of abuse/stalking/revenge porn, marginalized communities, political dissidents — for whom data persistence creates existential risk. The erasure right is not merely convenient but protective. They have the least capacity to navigate complex request processes or appeal denials. Their exit from the data ecosystem is effectively impossible (trapped). The constraint's value is highest for this group; its failure is most consequential here. Low epistemic friction (simple, accessible request processes) is structurally critical for them.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, vulnerable_data_subjects, beneficiary,
    powerless, biographical, trapped, continental).

% NGOs, lawyers, researchers who use Article 17 strategically: filing representative actions, testing edge cases, building precedent, pushing regulatory guidance. They benefit from the right's existence as a lever for structural change. They also observe and document compliance failures. Their exit is analytical — they engage by choice, not necessity. They amplify the beneficiary signal for data_subjects_general.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, digital_rights_advocates, beneficiary,
    organized, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__privacy_fundamental_reading, digital_rights_advocates, observer).

% Google, Meta, Microsoft, Amazon, Apple — operators of search, social, cloud, and marketplace platforms processing billions of data subjects' data. They design and build the erasure technical infrastructure (automated portals, backend purge pipelines, third-party notification systems). They bear the majority of absolute compliance costs. They shape implementation through technical standards, lobbying, and litigation. They can arbitrage jurisdictionally (data localization, processing relocation) and structurally (product redesign to minimize erasure scope). They are the primary constrained party but also the agenda-setters for how the constraint operates technically.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, large_platforms, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__privacy_fundamental_reading, large_platforms, payer).

% Small and medium enterprises, non-tech companies, public sector bodies, websites with user accounts — any organization processing personal data at modest scale. They must comply with erasure requests but lack the engineering capacity of large platforms. Per-request compliance cost is disproportionately high. They cannot easily exit data processing (customer data is operationally necessary). They are payers without agenda-setting power. The constraint's coordination benefit to them is minimal — they did not seek this obligation.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, smb_data_controllers, payer,
    moderate, biographical, constrained, regional).

% Data Protection Authorities (DPAs), EDPB, CJEU — they enforce Article 17 through fines, orders, guidelines, and precedent. They interpret the right's scope (backups, derived data, third parties, public interest exceptions). They set the enforcement tone that determines whether the constraint is rope (proportional, accessible) or drifts toward tangled_rope/snare (formalistic, punitive, captured). They bear enforcement costs but not operational compliance costs. Their exit is analytical — they are the constraint's institutional anchor.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__privacy_fundamental_reading, regulatory_authorities, observer).

% New search engines, social networks, data-intensive startups attempting to enter markets dominated by large_platforms. They must build Article 17 compliance from day one — a fixed cost that incumbents amortized over years. They have no voice in the constraint's design or evolution (excluded). They are trapped because the compliance infrastructure is a prerequisite for legal operation. If compliance cost asymmetry is real, this group is the primary victim of the competitive_moat_reading's extraction — but in this reading, they are collateral, not the primary extraction target.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, competing_platforms_entrants, excluded,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__privacy_fundamental_reading, competing_platforms_entrants, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, diffuse).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of individual helplessness against permanent, uncontrolled corporate data retention. Establishes a baseline: data controllers must erase personal data when its purpose is fulfilled, consent is withdrawn, or processing lacks lawful basis. Creates a legally enforceable lever for individuals over their data lifecycle, backed by regulatory enforcement — replacing bilateral power asymmetry with a standardized right.
% TRANSFER_FUNCTION: Moves compliance costs (erasure infrastructure, verification, legal review, regulatory risk) from data subjects to data controllers. Large platforms absorb the largest absolute costs; SMBs bear higher per-unit costs. Data subjects receive the coordination gain (control, erasure) without direct payment. The transfer is not monetary rent but operational burden shifted to the party with data control.
% ABSENT_VOICES: Future data subjects (children not yet online, populations in jurisdictions without similar rights) — they would benefit from stronger erasure but cannot advocate. Non-EU data subjects whose data touches EU controllers — they get derivative protection but no direct standing. Small publishers and archivists who fear erasure requests deleting public-interest content — they are not at the table but are affected by broad interpretation. The competitive_moat_reading's victim class (entrants) is structurally excluded from this reading's beneficiary set.
% DISAPPEARANCE_RATIONALE: If Article 17 vanished overnight, data subjects would lose their only legally enforceable lever over data retention. Controllers would revert to retention-by-default; erasure would become discretionary. The data economy would reorganize around permanent retention as norm. Vulnerable data subjects would lose critical protection. Regulatory enforcement would shift to weaker principles (storage limitation, purpose limitation) without the sharp erasure trigger. The coordination function would collapse.
% FOUNDING_PROBLEM: Pre-GDPR, individuals had no effective right to demand deletion of their personal data from corporate databases. Controllers retained data indefinitely by default; consent withdrawal did not trigger deletion; data portability did not exist. The power asymmetry was total: individuals generated the data but had zero control over its lifecycle. Article 17 was built to rebalance this asymmetry by making erasure a legal obligation, not a favor.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by: European Parliament legislative history (recitals 39, 65, 173); EDPS opinions on data retention necessity; academic literature on power asymmetry in data economies (Zuboff, Cohen, Hildebrandt); CJEU Google Spain reasoning (right to be forgotten as response to search engine permanence); NGO submissions during GDPR drafting (BEUC, EDRi, Access Now). No attestation comes solely from the beneficiary class — the corroboration is institutional, judicial, and scholarly, outside the data_subjects_general/vulnerable_data_subjects beneficiary set.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__privacy_fundamental_reading_tests).
:- end_tests(article17_erasure_right__privacy_fundamental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) reflects compliance overhead as a fraction of data-processing value — not zero because controllers must build erasure pipelines, verification flows, and audit trails, but not extractive in the rent sense because the costs fund a genuine coordination function (individual control over data). Suppression (0.18) is narrowly targeted at unlawful retention; lawful processing continues unhindered. Theater (0.22) captures the gap between formal compliance (automated portals, standard responses) and substantive erasure (backup purge, third-party propagation, derived data). Accessibility collapse (0.45) is moderate: alternatives to erasure (consent withdrawal, restriction, objection) exist but are less complete; the right does not eliminate the data economy. Resistance (0.38) reflects industry lobbying, technical obstruction, and narrow interpretations — real but not overwhelming. The claimed_type rope is structurally honest: this reading sees genuine coordination with net beneficiary gains and no alternative suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the data subject seat (especially vulnerable), the constraint is a rare rope — a genuine power rebalancing tool with low epistemic friction (as declared in the structural delta). From the large platform seat, it appears as a tangled_rope: coordination function real but compliance costs are high and shape product architecture. From the SMB controller seat, it leans snare-adjacent: costs are disproportionate, technical capacity limited, and the coordination benefit (to them) is near zero. From the entrant seat, it is a barrier (piton-adjacent): the constraint persists because incumbents can absorb costs that entrants cannot. The engine computes these per-seat divergences from the declared power/exit/role structures.
 *
 * DIRECTIONALITY LOGIC:
 *   data_subjects_general and vulnerable_data_subjects are declared beneficiaries — they receive the coordination gain (control over data) without bearing the infrastructure cost. Their exit_options are constrained (data_subjects_general) or trapped (vulnerable) — they cannot easily leave the data ecosystem, which is why the right must be legally enforced. large_platforms are agenda_setters (they design the technical implementation) but also payers (they fund the infrastructure) — their exit is arbitrage (they can restructure, lobby, relocate processing). smb_data_controllers are payers with constrained exit — they must comply but lack platform-scale infrastructure. competing_platforms_entrants are excluded/payers — they bear the entry barrier cost but have no voice in the constraint's design. Regulatory authorities are agenda_setters/observers with analytical exit — they enforce but do not bear operational costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (individual helplessness against permanent corporate data retention) remains live — data processing has only intensified since 2018. No mandatrophy: the constraint's function has not atrophied; if anything, its relevance has grown. The mandate (individual sovereignty over data) is still the operating logic, not a vestigial cover. Theater is rising (0.12→0.22) but from a low base — this is implementation maturity, not functional decay. The constraint is not a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is Article 17 a single constraint with multiple readings, or multiple structurally distinct constraints sharing a label?',
    'Apply the ε-invariance test: if different readings produce materially different ε values for the same enforcement machinery, they are different constraints. Compare extraction profiles of privacy_fundamental_reading vs competitive_moat_reading vs censorship_mechanism_reading.',
    'If structurally distinct, each reading becomes a separate constraint story linked via network.affects_constraints. If single constraint, the classification must account for multi-reading contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the Article 17 kernel decomposes into multiple ε-invariant constraints per the BGS decomposition principle.').

omega_variable(
    erasure_vs_freedom_of_expression_boundary,
    'Where does the erasure right''s legitimate scope end and content suppression begin?',
    'Court rulings on delisting vs takedown; balancing test jurisprudence (Google Spain, subsequent CJEU cases); platform transparency reports on request types and outcomes.',
    'If the boundary is narrow, the constraint is closer to rope (coordination of legitimate erasure). If broad, extraction and suppression rise toward tangled_rope or snare as speech is chilled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erasure_vs_freedom_of_expression_boundary, empirical, 'Structural boundary between privacy coordination and speech suppression in erasure enforcement.').

omega_variable(
    compliance_cost_asymmetry_magnitude,
    'Do compliance costs create a competitive moat that structurally benefits incumbents, or are they proportional to data-processing scale?',
    'Comparative cost analysis: erasure infrastructure cost per request for platforms of different sizes; market entry data for new search/social competitors post-GDPR.',
    'If costs are disproportionately burdensome for entrants, the competitive_moat_reading gains structural validity and this reading''s rope classification weakens toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_asymmetry_magnitude, empirical, 'Whether compliance cost asymmetry constitutes asymmetric extraction benefiting incumbents.').

omega_variable(
    technical_friction_of_broad_erasure,
    'What is the actual epistemic and technical friction for data subjects exercising broad erasure rights?',
    'User studies on request success rates, time-to-completion, partial compliance rates; platform API documentation and response latency measurements.',
    'High friction (complex verification, partial compliance, appeal loops) raises effective suppression and extraction for data subjects, moving classification toward snare/tangled_rope. Low friction supports rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technical_friction_of_broad_erasure, empirical, 'Operational accessibility of the erasure right for the declared beneficiary class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art17_priv_fund_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(art17_priv_fund_tr_t0, observed).
narrative_ontology:measurement(art17_priv_fund_tr_t4, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement_basis(art17_priv_fund_tr_t4, observed).
narrative_ontology:measurement(art17_priv_fund_tr_t8, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(art17_priv_fund_tr_t8, observed).
narrative_ontology:measurement(art17_priv_fund_tr_t12, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(art17_priv_fund_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(art17_priv_fund_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(art17_priv_fund_be_t0, observed).
narrative_ontology:measurement(art17_priv_fund_be_t4, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement_basis(art17_priv_fund_be_t4, observed).
narrative_ontology:measurement(art17_priv_fund_be_t8, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement_basis(art17_priv_fund_be_t8, observed).
narrative_ontology:measurement(art17_priv_fund_be_t12, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement_basis(art17_priv_fund_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(art17_priv_fund_su_t0, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(art17_priv_fund_su_t0, observed).
narrative_ontology:measurement(art17_priv_fund_su_t4, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 4, 0.12).
narrative_ontology:measurement_basis(art17_priv_fund_su_t4, observed).
narrative_ontology:measurement(art17_priv_fund_su_t8, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 8, 0.15).
narrative_ontology:measurement_basis(art17_priv_fund_su_t8, observed).
narrative_ontology:measurement(art17_priv_fund_su_t12, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 12, 0.18).
narrative_ontology:measurement_basis(art17_priv_fund_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, information_standard).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__privacy_fundamental_reading, 0.02).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, gdpr_data_portability_right).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, gdpr_right_to_restriction).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, eprivacy_regulation_coordination).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, digital_markets_act_interoperability).

% DUAL FORMULATION NOTE:
% Article 17 kernel decomposes into three readings with distinct ε profiles: privacy_fundamental_reading (ε≈0.32, rope) — coordination of individual data control; competitive_moat_reading (ε≈0.55+, tangled_rope) — coordination of data portability/erasure with asymmetric extraction via compliance cost moats; censorship_mechanism_reading (ε≈0.65+, snare/tangled_rope) — extraction via speech suppression weaponizing privacy process. The three constraints share enforcement machinery but have different beneficiary/victim structures and claimed types. This decomposition follows the BGS ε-invariance principle: different observables (individual control vs market structure vs speech outcomes) yield different ε, therefore different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__privacy_fundamental_reading, institutional, 0.35).
constraint_indexing:directionality_override(article17_erasure_right__privacy_fundamental_reading, moderate, 0.65).
constraint_indexing:directionality_override(article17_erasure_right__privacy_fundamental_reading, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
