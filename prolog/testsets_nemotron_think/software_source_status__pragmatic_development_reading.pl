% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Pragmatic Open Source Superiority Claim
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   The pragmatic development reading of software source status asserts that
 *   open source is a superior development methodology because transparency,
 *   peer review, and distributed collaboration produce better software
 *   faster. Freedom (to use, study, modify, share) is valued instrumentally —
 *   as the enabling condition for these quality mechanisms — not as an
 *   inherent right. This reading explicitly rejects the claim that
 *   proprietary software is inherently illegitimate, and accepts permissive
 *   licensing as compatible with, even desirable for, open source's pragmatic
 *   goals. The constraint is the discourse-level claim that this framing is
 *   the correct or sufficient justification for open source. It coordinates
 *   industry, developers, and users around a quality-outcome vocabulary while
 *   displacing the freedom-as-right vocabulary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.35).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.25).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Pragmatic Open Source Superiority Claim").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, '9b747d78-743a-4363-a918-e191d10f230a').
narrative_ontology:cs_kernel_codification('9b747d78-743a-4363-a918-e191d10f230a', distributed).
narrative_ontology:cs_authority_grounding('9b747d78-743a-4363-a918-e191d10f230a', expertise).
narrative_ontology:cs_reading_relation('9b747d78-743a-4363-a918-e191d10f230a', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b747d78-743a-4363-a918-e191d10f230a', software_source_status__property_rights_reading, influences).
narrative_ontology:cs_reading_relation('9b747d78-743a-4363-a918-e191d10f230a', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('9b747d78-743a-4363-a918-e191d10f230a', foundational, open_source_superior_methodology).
narrative_ontology:cs_axiom_status(open_source_superior_methodology, holdable).
narrative_ontology:cs_axiom_grounding('9b747d78-743a-4363-a918-e191d10f230a', open_source_superior_methodology, empirically_contingent).
narrative_ontology:cs_axiom('9b747d78-743a-4363-a918-e191d10f230a', foundational, freedom_instrumental_to_quality).
narrative_ontology:cs_axiom_status(freedom_instrumental_to_quality, holdable).
narrative_ontology:cs_axiom_grounding('9b747d78-743a-4363-a918-e191d10f230a', freedom_instrumental_to_quality, instrumental).
narrative_ontology:cs_axiom('9b747d78-743a-4363-a918-e191d10f230a', secondary, permissive_licensing_maximizes_adoption).
narrative_ontology:cs_axiom_status(permissive_licensing_maximizes_adoption, holdable).
narrative_ontology:cs_axiom_grounding('9b747d78-743a-4363-a918-e191d10f230a', permissive_licensing_maximizes_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('9b747d78-743a-4363-a918-e191d10f230a', pragmatic_development_frame).
narrative_ontology:cs_drift_state('9b747d78-743a-4363-a918-e191d10f230a', contemporary_open_source_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9b747d78-743a-4363-a918-e191d10f230a', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_source_pragmatists).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, permissive_license_advocates).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, industry_open_source_programs).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, freedom_imperative_advocates).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, pragmatic_open_source_thesis).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, peer_review_improves_quality).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, permissive_licensing_enables_adoption).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, open_development_accelerates_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for open source on practical grounds — peer review, bug detection, faster iteration. They frame freedom as a means to quality, not an end. They founded the Open Source Initiative (1998) to rebrand free software for business adoption. Their influence shapes licensing norms, governance models, and corporate open source strategy.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_pragmatists, agenda_setter,
    organized, biographical, mobile, global).

% Promote BSD, MIT, Apache licenses over copyleft. They benefit when the pragmatic framing validates permissive licensing as 'business-friendly' and 'maximizing adoption.' Their preferred licenses become default for new projects, corporate releases, and platform infrastructure.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, permissive_license_advocates, beneficiary,
    organized, biographical, mobile, global).

% Corporate open source offices (Google, Microsoft, Meta, etc.) that fund, release, and govern open source projects. They adopt the pragmatic framing to justify investment without copyleft obligations. They capture value through talent recruitment, ecosystem control, and standards influence while avoiding 'viral' license requirements.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, industry_open_source_programs, beneficiary,
    institutional, biographical, arbitrage, global).

% End users and developer-users who receive higher-quality software through open development practices. They benefit from the pragmatic framing's emphasis on reliability, security, and feature velocity. Their exit is constrained by platform lock-in and network effects — they cannot easily switch to alternative software stacks.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_users, beneficiary,
    moderate, biographical, constrained, global).

% Free software advocates (FSF, GNU project, copyleft proponents) who hold that user freedom is a non-negotiable ethical requirement. The pragmatic framing displaces their moral vocabulary, renders copyleft 'ideological,' and enables permissive licensing that permits proprietary forks. They bear the cost of arguing for freedom in a discourse that treats it as instrumental. Their identity is fused to the freedom frame — exit means abandoning their core commitment.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, freedom_imperative_advocates, payer,
    organized, generational, identity_locked, global).

% Traditional closed-source companies (Oracle, Adobe, legacy ISVs) that watch the pragmatic open source framing. The reading explicitly says proprietary software is not inherently illegitimate, so they are not targets. They observe to calibrate their own source-available, fair-code, or hybrid strategies. They can engage or ignore at will.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_vendors, observer,
    institutional, biographical, arbitrage, global).

% Scholars of software engineering, commons-based peer production, and intellectual property who study the pragmatic framing's empirical claims — does open development actually produce better quality? They provide evidence that feeds back into the discourse but do not directly collect or pay.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, academic_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates software development around peer review, transparency, and collaborative improvement as quality mechanisms rather than moral imperatives. Solves the problem of how to produce reliable, innovative software at scale by aligning developer incentives with open processes.
% TRANSFER_FUNCTION: Moves legitimacy and adoption from freedom-based to outcome-based justifications for open source. Moves licensing preference toward permissive licenses (MIT, Apache, BSD) and away from copyleft (GPL). Moves corporate investment into open source by removing the 'viral license' barrier.
% ABSENT_VOICES: Copyleft purists who see permissive licensing as enabling proprietary capture of community labor. Users in locked-down ecosystems (iOS, game consoles, embedded devices) who experience 'open source' components without the freedom to modify their own devices. Global South developers for whom the pragmatic framing's corporate-centric governance reproduces extractive dynamics.
% DISAPPEARANCE_RATIONALE: If the pragmatic framing vanished overnight, the freedom framing would reclaim centrality in open source discourse. Copyleft adoption would likely rise. Corporate open source programs would face stronger pressure to use reciprocal licenses. The 'open core' and 'source available' strategies that rely on 'permissive = pragmatic' justification would lose their primary rhetorical cover.
% FOUNDING_PROBLEM: The free software movement's moral framing (freedom as fundamental right) limited adoption in commercial contexts. Businesses rejected GPL as 'viral' and ideologically suspect. A pragmatic framing was needed to demonstrate open source's practical value — lower defect rates, faster innovation, vendor neutrality — without requiring moral commitment to user freedom.
% FOUNDING_PROBLEM_CORROBORATION: Eric Raymond's 'The Cathedral and the Bazaar' (1997) and the founding of the Open Source Initiative (1998) explicitly pivoted from moral to pragmatic framing. Business adoption histories (Linux in enterprise, Android, cloud infrastructure) corroborate that pragmatic arguments unlocked commercial investment. The persistent tension — FSF's continued insistence on 'free software,' GPLv3's anti-tivoization clause, the 'open core' controversy — confirms the founding problem remains live.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).
:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects the displacement of the freedom framing's moral authority — the pragmatic reading extracts legitimacy from the freedom movement without adopting its commitments. Suppression (0.25) is low but non-zero: the framing renders freedom talk 'ideological' and 'impractical,' creating soft pressure against copyleft advocates in corporate and standards settings. Theater ratio (0.18) is low — proponents genuinely believe the empirical claims. Accessibility collapse (0.35) is moderate: the freedom framing persists (FSF, GPL, AGPL, ethical licensing movements) but operates in a discursive environment where pragmatic language dominates. Resistance (0.45) reflects active pushback from freedom advocates and ethical licensing movements. The claimed type is rope: a coordination mechanism (pragmatic justification) that solves a collective action problem (commercial adoption of open development) with minimal coercion and net benefit to participants.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (pragmatists), the constraint is a successful coordination rope: it opened the door for massive value creation. From the payer seat (freedom advocates), the same structure operates as extraction — their life's work (the moral framing) is repurposed as a marketing term while its substance is hollowed out. The engine computes this divergence from the structural data; the authored claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Open source pragmatists and industry programs are structural beneficiaries (d near 0.0) — they collect adoption, investment, and talent. Permissive license advocates benefit from licensing norm shifts. Software users benefit from quality outcomes but are constrained by platform lock-in (d ~0.4). Freedom imperative advocates are payers (d ~0.8) — their moral vocabulary is displaced, their copyleft tools marginalized as 'ideological,' and their identity commitment makes exit nearly impossible (identity_locked). Proprietary vendors and academics are observers — the reading explicitly does not target proprietary software, and academics study rather than participate in the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (commercial adoption blocked by moral framing) was real and the pragmatic framing solved it. But the arrangement persists beyond the original coordination need: open source is now dominant infrastructure. The pragmatic framing now serves to legitimize permissive licensing that enables proprietary capture — a function not in the original mandate. This is mandatrophy: the constraint's mandate (enable adoption) has been achieved, but the framing persists because it now benefits powerful institutional actors (industry programs) who would lose leverage under copyleft norms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pragmatic_framing_causal_role,
    'Did the pragmatic framing causally enable commercial open source adoption, or did it merely rationalize adoption that would have occurred anyway due to economic pressures?',
    'Counterfactual analysis of early corporate Linux adoption (late 1990s): compare firms exposed to OSI messaging vs. those adopting for purely technical reasons. Natural experiment in embedded systems where GPL was unavoidable (kernel) vs. userspace where permissive licensing prevailed.',
    'If causal, the framing is a genuine coordination innovation (rope). If post-hoc rationalization, the framing is extractive cover for corporate capture of commons labor (tangled_rope/snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatic_framing_causal_role, empirical, 'Causal efficacy of the pragmatic rebranding.').

omega_variable(
    permissive_licensing_capture_risk,
    'Does the pragmatic reading''s validation of permissive licensing enable proprietary capture of open source contributions (the ''AWS problem'': cloud providers monetize open source without reciprocating)?',
    'Track contribution flows and revenue capture in permissive vs. copyleft ecosystems over time. Measure whether projects that relicense from permissive to copyleft (or source-available) cite capture as motivation.',
    'If yes, the pragmatic reading''s coordination function masks an extraction mechanism — companies benefit from community labor without reciprocity (tangled_rope). If no, permissive licensing genuinely maximizes total welfare (rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(permissive_licensing_capture_risk, empirical, 'Whether permissive licensing validated by this reading enables asymmetric value capture.').

omega_variable(
    methodology_superiority_scope,
    'Does ''open source is a superior development methodology'' hold universally, or only in specific domains (kernels, web infrastructure, developer tools) where modularity and parallelism are high?',
    'Meta-analysis of defect density, velocity, and innovation metrics across open vs. closed projects in different domains (OS kernels, games, embedded, scientific computing, SaaS). Control for team size, funding, and domain complexity.',
    'If domain-limited, the universal claim is overreach — the constraint presents a contingent empirical regularity as a natural law (false_summit_mountain candidate). If universal, the claim is a genuine coordination truth (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_superiority_scope, empirical, 'Scope conditions of the pragmatic superiority claim.').

omega_variable(
    kernel_reading_relations,
    'What are the structural relationships between this pragmatic reading and the sibling readings of the software_source_status kernel?',
    'Map the logical and institutional dependencies: does the pragmatic reading''s dominance in industry create material conditions that undermine the freedom reading''s viability (influences)? Can a single actor coherently hold both pragmatic and freedom frames (coexists_with)? Does the property_rights reading''s legitimacy depend on the pragmatic reading''s existence as a ''reasonable'' open source alternative (influences)?',
    'Determines whether the kernel''s readings form a stable coexistence, a competitive displacement, or a structured hierarchy. Feeds cs_structure.reading_relations and the engine''s cross-reading contamination analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationships among kernel sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(software_source_status_pragmatic_tr_t1998, software_source_status__pragmatic_development_reading, theater_ratio, 1998, 0.05).
narrative_ontology:measurement(software_source_status_pragmatic_tr_t2004, software_source_status__pragmatic_development_reading, theater_ratio, 2004, 0.08).
narrative_ontology:measurement(software_source_status_pragmatic_tr_t2010, software_source_status__pragmatic_development_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(software_source_status_pragmatic_tr_t2016, software_source_status__pragmatic_development_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(software_source_status_pragmatic_tr_t2020, software_source_status__pragmatic_development_reading, theater_ratio, 2020, 0.17).
narrative_ontology:measurement(software_source_status_pragmatic_tr_t2024, software_source_status__pragmatic_development_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(software_source_status_pragmatic_be_t1998, software_source_status__pragmatic_development_reading, base_extractiveness, 1998, 0.15).
narrative_ontology:measurement(software_source_status_pragmatic_be_t2004, software_source_status__pragmatic_development_reading, base_extractiveness, 2004, 0.22).
narrative_ontology:measurement(software_source_status_pragmatic_be_t2010, software_source_status__pragmatic_development_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement(software_source_status_pragmatic_be_t2016, software_source_status__pragmatic_development_reading, base_extractiveness, 2016, 0.32).
narrative_ontology:measurement(software_source_status_pragmatic_be_t2020, software_source_status__pragmatic_development_reading, base_extractiveness, 2020, 0.34).
narrative_ontology:measurement(software_source_status_pragmatic_be_t2024, software_source_status__pragmatic_development_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(software_source_status_pragmatic_su_t1998, software_source_status__pragmatic_development_reading, suppression_requirement, 1998, 0.1).
narrative_ontology:measurement(software_source_status_pragmatic_su_t2004, software_source_status__pragmatic_development_reading, suppression_requirement, 2004, 0.15).
narrative_ontology:measurement(software_source_status_pragmatic_su_t2010, software_source_status__pragmatic_development_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(software_source_status_pragmatic_su_t2016, software_source_status__pragmatic_development_reading, suppression_requirement, 2016, 0.23).
narrative_ontology:measurement(software_source_status_pragmatic_su_t2020, software_source_status__pragmatic_development_reading, suppression_requirement, 2020, 0.24).
narrative_ontology:measurement(software_source_status_pragmatic_su_t2024, software_source_status__pragmatic_development_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, information_standard).
narrative_ontology:boltzmann_floor_override(software_source_status__pragmatic_development_reading, 0.02).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, corporate_open_source_strategy).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, copyleft_adoption_dynamics).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, ethical_licensing_movement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the software_source_status kernel. The kernel decomposes into four readings with distinct ε values and beneficiary/victim structures. The pragmatic reading (this file) has ε=0.35 (rope). The freedom_imperative reading has higher ε (displaces proprietary developers as victims). The property_rights reading has near-zero ε for proprietary developers but high ε for users. The utilitarian_hybrid reading has context-dependent ε. They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__pragmatic_development_reading, organized, 0.15).
constraint_indexing:directionality_override(software_source_status__pragmatic_development_reading, institutional, 0.1).
constraint_indexing:directionality_override(software_source_status__pragmatic_development_reading, moderate, 0.4).
constraint_indexing:directionality_override(software_source_status__pragmatic_development_reading, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
