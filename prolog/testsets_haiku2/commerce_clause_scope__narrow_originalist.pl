% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause: Narrow Originalist Reading (Interstate Trade Facilitation)
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The narrow_originalist reading of the Commerce Clause interprets
 *   'regulate Commerce among the several States' to mean federal power is
 *   limited to removing state-imposed barriers to interstate trade and
 *   ensuring uniform rules for actual cross-border commerce. 'Regulate' means
 *   facilitate, not restrict or comprehensively control. This reading rejects
 *   the post-1942 broad_effects_test as constitutional overreach: federal
 *   power does not extend to comprehensive regulation of intrastate economic
 *   activity (e.g., agriculture, manufacturing, local services) even when
 *   those activities cumulatively affect interstate commerce, nor to
 *   non-commercial activity (environmental protection, civil rights, labor
 *   standards) regardless of economic impact. Under this reading, civil
 *   rights enforcement, environmental protection, and labor regulation fall
 *   outside federal constitutional authority unless they directly concern
 *   interstate commerce channels. The reading preserves state sovereignty
 *   over the vast bulk of domestic economic and social regulation and returns
 *   authority for civil rights and environmental protection to state
 *   governments—where protections vary widely and are often weak in
 *   recalcitrant jurisdictions. This constraint story instantiates the
 *   narrow_originalist reading as a coherent, defended constitutional
 *   position—not as the governing law (which is closer to broad_effects_test
 *   or intermediate_channels), but as a live alternative in constitutional
 *   contestation.
 *
 * KEY AGENTS:
 *   - state_governments — institutional beneficiaries; retain sovereign regulatory authority under this reading
 *   - local_businesses — organized beneficiaries; face narrower federal regulatory reach
 *   - decentralized_regulatory_communities — organized beneficiaries/observers; advocate for federalism and state-level experimentation
 *   - interstate_commerce_operators — powerful mixed role; benefit from a clear rule for interstate trade but remain subject to state regulations for non-commerce activities
 *   - civil_rights_constituencies — powerless victims; lose federal legal remedies in states with weak protections
 *   - federal_environmental_advocates — organized victims; federal environmental authority is narrowed to interstate harms only
 *   - federal_regulatory_agencies — institutional victims/observers; their statutory authority is narrowed or invalidated
 *   - Congress — institutional agenda-setter; retains power to regulate interstate commerce and remove state barriers, but cannot regulate purely intrastate activity
 *   - Supreme_Court — institutional observer/arbiter; mediates constitutional interpretation
 *   - progressive_regulatory_advocates — excluded; systematically excluded from the reading's framework by its core axiom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.28).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.15).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.28).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause: Narrow Originalist Reading (Interstate Trade Facilitation)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional/federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '4111ba25-ee2d-4bff-a56e-d4e67ea7220a').
narrative_ontology:cs_kernel_codification('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', formalized).
narrative_ontology:cs_authority_grounding('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', lineage).
narrative_ontology:cs_interpretation_layer_present('4111ba25-ee2d-4bff-a56e-d4e67ea7220a').
narrative_ontology:cs_reading_relation('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_reading_relation('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', foundational, regulate_means_facilitate_not_control).
narrative_ontology:cs_axiom_status(regulate_means_facilitate_not_control, holdable).
narrative_ontology:cs_axiom_grounding('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', regulate_means_facilitate_not_control, empirically_contingent).
narrative_ontology:cs_axiom('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', foundational, enumerated_powers_exclude_non_commerce_activity).
narrative_ontology:cs_axiom_status(enumerated_powers_exclude_non_commerce_activity, holdable).
narrative_ontology:cs_axiom_grounding('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', enumerated_powers_exclude_non_commerce_activity, deontological).
narrative_ontology:cs_reference_frame('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', original_constitutional_federalism).
narrative_ontology:cs_drift_state('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', post_1942_commerce_power_expansion, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('4111ba25-ee2d-4bff-a56e-d4e67ea7220a', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_regulatory_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, interstate_commerce_operators).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, interstate_commerce_operators).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_constituencies).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_environmental_protection_advocates).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, constitutional_federalism_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, enumerated_powers_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain broad police powers over intrastate economic activity, labor standards, environmental regulation, and civil rights within their borders. Under this reading, the federal Commerce Clause does not authorize preemption of state laws regulating non-commercial or purely local activities. States benefit from autonomy to set their own regulatory regimes and preserve the constitutional balance of enumerated federal powers.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, analytical, national).

% Face a narrower scope of federal regulatory reach: federal environmental, labor, and civil rights rules that apply based on cumulative economic effect do not bind them under this reading. They operate under state and local rules only, with lower compliance burden for federal standards that would apply under broader readings. Their regulatory environment is set by state legislatures, not national agencies.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    organized, biographical, constrained, regional).

% Advocates for federalism and local regulatory experimentation (state tort law, contract law, occupational licensing, environmental quality standards set by states). They frame state-level diversity as a source of innovation and democratic legitimacy. Federal preemption of state regulatory schemes is treated as unconstitutional overreach under this reading.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, decentralized_regulatory_communities, beneficiary,
    organized, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, decentralized_regulatory_communities, observer).

% Benefit from a clear, narrow rule: federal law may not burden goods or services actually crossing state lines, and must ensure uniform rules for interstate trade itself (interstate shipping, multi-state contracts, federal channels). They face potential compliance challenges if states impose conflicting regulations on interstate transactions, but this reading limits federal preemption authority, leaving state-level regulatory burdens in place for non-commerce-clause-enumerated domains.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, interstate_commerce_operators, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, interstate_commerce_operators, payer).

% Cannot invoke federal civil rights law to challenge state or private discrimination in public accommodations, employment, or housing when the regulated activity is non-commercial or lacks a substantial-effects connection to interstate commerce. Under this reading, laws like the Civil Rights Act of 1964 (as applied to local public accommodations and employment) and the Fair Housing Act exceed enumerated federal power. They depend on state-level civil rights protections, which vary widely and are often weak in recalcitrant jurisdictions.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_constituencies, payer,
    powerless, biographical, trapped, national).

% Under this reading, federal environmental regulation of non-commerce-related activities (e.g., wetland preservation, endangered species protection on private land, air quality standards for intrastate sources) exceeds constitutional authority. They must rely on state environmental law, which is often weaker or inconsistent across jurisdictions. Interstate environmental harms (pollution crossing state lines) fall under federal authority, but purely intrastate environmental degradation does not.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_environmental_protection_advocates, payer,
    organized, generational, constrained, global).

% Their statutory authority to regulate national economic and social conditions is narrowed under this reading. Agencies like the EPA, OSHA, EEOC, and NLRB operate under the assumption that the Commerce Clause authorizes comprehensive regulation of economic activity and its cumulative effects. This reading would invalidate many of their regulatory programs as unconstitutional. They remain as institutional actors but with sharply reduced constitutional jurisdiction.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies, payer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies, observer).

% Under this reading, Congress retains enumerated power to regulate interstate commerce and remove state-imposed barriers (e.g., state tariffs, discriminatory state regulations that burden interstate trade). Congress cannot use the Commerce Clause to regulate intrastate activity, non-commercial activity, or activity without a direct connection to trade crossing state lines. Congress sets the terms by passing legislation within its enumerated powers.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, congress_enacting_body, agenda_setter,
    institutional, generational, analytical, national).

% Acts as the authoritative arbiter of constitutional meaning. This reading represents one live interpretive position held by some Justices and legal scholars. The Court's actual doctrine (post-1942) is closer to the broad_effects_test or intermediate_channels readings. This story represents the narrow_originalist reading as a coherent, defended alternative within ongoing constitutional contestation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, supreme_court_interpretive_body, observer,
    institutional, generational, analytical, national).

% Would argue that federal power must extend to comprehensive regulation of economic activity to address national problems (labor standards, environmental protection, civil rights). They are not parties to the narrow_originalist reading's institutional framing—their voice is excluded by the reading's core premise that enumerated powers do not reach national economic coordination beyond interstate-trade facilitation. They would reject the reading's legitimacy altogether.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, progressive_regulatory_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_scope__narrow_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a constitutional rule for federal-state boundary: federal power is limited to removing state-imposed barriers to interstate trade and ensuring uniform rules for commerce crossing state lines. State governments retain sovereign authority over intrastate economic and social regulation. The rule solves the coordination problem of allocating regulatory jurisdiction between federal and state sovereigns without creating duplicative, conflicting regulatory structures.
% TRANSFER_FUNCTION: Transfers regulatory authority from federal agencies to state governments and from national civil-rights law to state-law protections. It shifts the locus of standard-setting for labor, environmental, and civil-rights policy from federal statutes and agencies to state legislatures and agencies. Non-commercial activity—including much of civil rights protection—is removed from federal regulatory reach and returns to state control.
% ABSENT_VOICES: Constituencies that depend on federal civil rights enforcement (racial minorities, women, workers in historically discriminatory jurisdictions) and environmental protection advocates who depend on federal environmental law would reject this reading's premises. They are systematically excluded from the narrow_originalist framework because the reading's core axiom—that the Commerce Clause does not authorize federal regulation of non-commercial or purely intrastate activity—forecloses their claims for federal protection. They have no seat at the originalist table because the reading does not recognize the constitutional basis for federal authority they invoke.
% DISAPPEARANCE_RATIONALE: If this narrow reading became the operative constitutional law, federal civil rights statutes (Civil Rights Act of 1964 as applied to intrastate public accommodations, Fair Housing Act, portions of the Americans with Disabilities Act), federal environmental laws (Clean Water Act, Clean Air Act, Endangered Species Act as applied to non-interstate species), and federal labor law (NLRB jurisdiction over many intrastate businesses, OSHA standards for intrastate workplace hazards) would be struck down as unconstitutional. States would resume full regulatory authority over these domains. The regulatory landscape would fragment into fifty state regimes with varying civil-rights, environmental, and labor protections. Businesses would face a patchwork of state rules instead of uniform national standards. National civil-rights constituencies would lose the federal legal remedies they depend on in states with weak or hostile civil-rights laws.
% FOUNDING_PROBLEM: The Constitution delegates enumerated powers to Congress and reserves all others to the states. The Commerce Clause grants Congress power to 'regulate Commerce among the several States.' The founding problem is to discern the boundaries of this delegation: what does 'regulate' mean, what counts as 'Commerce,' and what is 'among the several States'? The narrow reading interprets 'regulate' as making regular/facilitating trade and limits 'Commerce among the States' to the actual crossing of goods and services across state lines.
% FOUNDING_PROBLEM_CORROBORATION: The narrow_originalist reading is defended by originalist scholars (Randy Barnett, Ilya Somin) and cited by some Justices (Clarence Thomas has invoked narrower readings in opinions). However, post-1942 Supreme Court doctrine and the vast majority of constitutional law scholars support broader readings (broad_effects_test or intermediate_channels). The National Labor Relations Board, EPA, EEOC, and other federal agencies operate under the assumption that the Commerce Clause authorizes comprehensive economic regulation. Congress has enacted major civil rights and environmental statutes under commerce power authority. This consensus—anchored in actual doctrine and regulatory practice since the 1940s—contradicts the narrow reading's claim that the founding problem remains 'live' as originally understood. The narrow_originalist reading remains a live position in constitutional scholarship and jurisprudence but is not the governing law.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading produces LOW extractiveness (0.28 at interval end) because it does not concentrate power asymmetrically—federal authority is narrowed, state authority is retained, and the rule is symmetrical (all states operate under the same federal constraint, all retain the same sovereign sphere). Suppression is LOW (0.15) because there is minimal coercive enforcement machinery: the reading operates as a constitutional boundary, not as an actively-policed regime. Theater_ratio is modest (0.22 at interval end) because the reading has genuine substantive content (enumerated powers do mean something), but its operation cannot be directly observed—constitutional jurisprudence is theatrical by nature (judicial opinions, law-review arguments, academic debates). The RISE in all three metrics over the interval (1950–2024) reflects the growing gap between the narrow_originalist reading and actual governing doctrine: as federal regulatory authority has expanded and stabilized (1950–1975) and then remained stable but contested (1975–2024), the narrow_originalist reading has become increasingly a position defended against the legal status quo rather than a description of governing rules. The theater_ratio rises because the reading's proponents must argue increasingly vociferously against the established doctrine; suppression_requirement rises because suppressing the narrow reading (via adverse Supreme Court precedent) is what holds the current doctrine in place. The measurement interval spans the period from the post-1942 Constitutional Revolution (when the Court abandoned the narrow reading) through the present-day revival of originalist arguments (especially in Thomas's opinions, starting circa 2000). The metrics are measured on ONE SHARED TIME GRID, which means every time point carries every metric.
 *
 * PERSPECTIVAL GAP:
 *   State governments see this reading as a restoration of constitutional legitimacy and proper federalism. Federal regulatory agencies see it as an unconstitutional seizure of authority they believe Congress properly delegated. Civil rights constituencies see it as abandoning their protection to state governments that have historically resisted enforcement. Originalist scholars see it as recovering the Constitution's original meaning. Broad_effects_test proponents see it as an anachronism that prevents federal coordination of national problems. The engine computes these perspectival divergences from the structural data: the state-government seat benefits structurally from the narrow reading (high d toward beneficiary end), while the civil-rights seat is targeted (high d toward victim end). The narrow originalist reading does not itself resolve these divergences—it instantiates one side of the contest.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are the structural beneficiaries (d ≈ 0.1–0.2): they retain regulatory sovereignty under this reading and are not subject to federal preemption for most domains. Local businesses are secondary beneficiaries (d ≈ 0.2–0.3): they face a narrower federal regulatory footprint but remain subject to state regulation. Civil rights constituencies and environmental advocates are the structural victims (d ≈ 0.7–0.8): they lose federal legal authority to enforce civil rights and environmental standards in states with weak protections. Federal regulatory agencies are institutional victims (d ≈ 0.6–0.7): their statutory authority is narrowed or invalidated. Congress and the Supreme Court are observers/arbiters (d ≈ 0.5): they are institutional mediators, not themselves extracted from or benefiting—though Congress might be read as an agenda-setter (d ≈ 0.3) because it retains power to enact interstate-commerce legislation. The directionality distribution is NOT extreme: the reading does not concentrate extraction on a small powerless group; rather, it redistributes authority from federal agencies and civil-rights constituencies to state governments. This is why extractiveness is modest (0.28) rather than high—the constraint is asymmetrical (benefiting some, burdening others) but not dramatically coercive.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow_originalist reading claims to preserve the original constitutional balance of enumerated powers and state sovereignty. However, post-1942 doctrine has moved decisively away from this reading, and actual federal regulatory authority has expanded and stabilized. The founding problem—the meaning of 'Commerce among the several States'—is CONTESTED (three live readings) rather than solved. The narrow_originalist reading has NOT resolved the contest; instead, it has become an insurgent position against the established doctrine. Mandatrophy analysis: the reading's mandate (preserve state sovereignty and limit federal power to its enumerated scope) remains LIVE as a normative claim held by federalist constituencies and originalist scholars. But the reading has not captured policy or governing doctrine; it operates as a constitutional objection to the status quo rather than as the status quo itself. The reading prevents mislabeling federal commerce-power regulation as 'natural law' or unconditionally legitimate (by insisting it is a contestable constitutional choice), but it does not itself prevent mislabeling the current doctrine as inevitable or non-extractive. No mandatrophy verdict is triggered here: the narrow_originalist reading is a coherent, defended alternative, not a zombie constraint. The status quo doctrine (broad_effects_test / intermediate_channels) may itself be subject to mandatrophy analysis (is the comprehensive federal regulatory apparatus still coordinating, or has it become extractive?), but that is a different story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_meaning_of_regulate,
    'Does the Constitution''s use of ''regulate Commerce'' mean ''facilitate and make regular'' (narrow reading) or ''exercise authority over / comprehensively control'' (broad reading)?',
    'Originalist historical analysis (18th-century usage of ''regulate''), comparison with other uses of the term in the Constitution, and evidence of Framers'' intent from contemporaneous sources.',
    'If ''regulate'' means facilitate only, the narrow reading holds and federal power is narrowly limited. If ''regulate'' includes prohibition and comprehensive control, the broad_effects_test or intermediate_channels readings are supported. This is the semantic crux of the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_meaning_of_regulate, conceptual, 'Semantic ambiguity in the Constitution''s use of ''regulate'' — does it mean facilitate or comprehensively control?').

omega_variable(
    intrastate_vs_interstate_boundary,
    'Is the distinction between intrastate and interstate commerce a stable, constitutionally meaningful line, or is it erased by the doctrine that intrastate activity substantially affecting interstate commerce falls under federal authority?',
    'Empirical test: can intrastate activity ever be so local, so non-commercial, and so disconnected from interstate effects that it falls outside federal regulatory reach? Modern doctrine would say no (everything has some effect). The narrow reading says yes (truly local, non-commercial activity is beyond federal reach).',
    'If the boundary is stable and meaningful, the narrow_originalist reading has content and can limit federal power. If the boundary has been erased by the substantial-effects doctrine, the narrow reading is a conceptual artifact and federal power is effectively unlimited in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrastate_vs_interstate_boundary, empirical, 'Whether the intrastate/interstate boundary remains constitutionally meaningful post-1942.').

omega_variable(
    sovereignty_extraction_ambiguity,
    'Does the broad_effects_test reading of the Commerce Clause constitute extraction from state sovereignty (federal regulatory authority overreach), or is it a legitimate evolution of enumerated federal power?',
    'Normative evaluation of federalism theory: does a strong federal commerce power that permits regulation of intrastate activity with cumulative interstate effects represent an illegitimate seizure of state authority, or a rational coordination response to an integrated national economy?',
    'If the broad_effects_test is extractive (seizing state authority unjustly), the narrow_originalist reading is vindicated as a protective constraint and the current doctrine is a Snare. If the broad_effects_test is legitimate coordination, the narrow_originalist reading is anachronistic and the current doctrine is a Rope or Tangled Rope. This ambiguity maps onto the kernel contest itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_extraction_ambiguity, preference, 'Whether broad federal commerce-power authority is extractive overreach or legitimate coordination.').

omega_variable(
    civil_rights_protection_locus,
    'Can civil rights protections (racial equality, gender equality, disability access) be adequately provided by state-level law, or does their vindication require federal statutory authority backed by Commerce Clause power?',
    'Empirical: compare the strength, uniformity, and enforcement of civil rights law across states with weak federal authority (pre-1964) versus states with federal authority backing (post-1964). Historical record shows substantial variation and undercoverage in states without federal pressure.',
    'If state-level civil rights protections are adequate, the narrow reading''s allocation of authority to states is workable. If state-level protections are inadequate or systematically weak in recalcitrant jurisdictions, the narrow reading sacrifices civil rights enforcement to federalism principles—making the reading''s extractiveness from civil-rights constituencies very high and the constraint classification potentially Snare rather than Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civil_rights_protection_locus, empirical, 'Whether civil rights enforcement is adequate under state-level authority alone, or requires federal backing.').

omega_variable(
    reading_vs_doctrine_gap,
    'Why has the narrow_originalist reading lost interpretive authority in constitutional law and been replaced by the broad_effects_test reading, despite originalist scholarly arguments that the narrow reading is faithful to the constitutional text?',
    'Historical analysis of the Constitutional Revolution of 1942 and subsequent doctrine. What institutional, political, or economic forces drove the shift? Was it genuine constitutional reinterpretation, political realignment, or institutional failure of originalist arguments?',
    'If the shift was justified by genuine constitutional reinterpretation or changed circumstances, the narrow reading is superseded and theatre_ratio rises. If the shift was a power grab or institutional failure, the narrow reading remains live and its revival is possible. This affects the reading''s status in contemporary jurisprudence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_doctrine_gap, conceptual, 'Why the narrow_originalist reading was displaced by broader readings despite textual and originalist arguments in its favor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1950, commerce_clause_scope__narrow_originalist, theater_ratio, 1950, 0.05).
narrative_ontology:measurement_basis(comm_tr_t1950, observed).
narrative_ontology:measurement(comm_tr_t1965, commerce_clause_scope__narrow_originalist, theater_ratio, 1965, 0.08).
narrative_ontology:measurement_basis(comm_tr_t1965, observed).
narrative_ontology:measurement(comm_tr_t1980, commerce_clause_scope__narrow_originalist, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(comm_tr_t1980, observed).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__narrow_originalist, theater_ratio, 2000, 0.21).
narrative_ontology:measurement_basis(comm_tr_t2000, observed).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_scope__narrow_originalist, theater_ratio, 2010, 0.22).
narrative_ontology:measurement_basis(comm_tr_t2010, observed).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_scope__narrow_originalist, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(comm_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1950, commerce_clause_scope__narrow_originalist, base_extractiveness, 1950, 0.08).
narrative_ontology:measurement_basis(comm_be_t1950, observed).
narrative_ontology:measurement(comm_be_t1965, commerce_clause_scope__narrow_originalist, base_extractiveness, 1965, 0.18).
narrative_ontology:measurement_basis(comm_be_t1965, observed).
narrative_ontology:measurement(comm_be_t1980, commerce_clause_scope__narrow_originalist, base_extractiveness, 1980, 0.24).
narrative_ontology:measurement_basis(comm_be_t1980, observed).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__narrow_originalist, base_extractiveness, 2000, 0.27).
narrative_ontology:measurement_basis(comm_be_t2000, observed).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_scope__narrow_originalist, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement_basis(comm_be_t2010, observed).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_scope__narrow_originalist, base_extractiveness, 2024, 0.28).
narrative_ontology:measurement_basis(comm_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1950, commerce_clause_scope__narrow_originalist, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement_basis(comm_su_t1950, observed).
narrative_ontology:measurement(comm_su_t1965, commerce_clause_scope__narrow_originalist, suppression_requirement, 1965, 0.08).
narrative_ontology:measurement_basis(comm_su_t1965, observed).
narrative_ontology:measurement(comm_su_t1980, commerce_clause_scope__narrow_originalist, suppression_requirement, 1980, 0.12).
narrative_ontology:measurement_basis(comm_su_t1980, observed).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_scope__narrow_originalist, suppression_requirement, 2000, 0.14).
narrative_ontology:measurement_basis(comm_su_t2000, observed).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_scope__narrow_originalist, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement_basis(comm_su_t2010, observed).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_scope__narrow_originalist, suppression_requirement, 2024, 0.15).
narrative_ontology:measurement_basis(comm_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__narrow_originalist, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, federal_preemption_doctrine).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, civil_rights_act_1964_constitutionality).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, environmental_protection_agency_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the commerce_clause_scope kernel. The broad_effects_test and intermediate_channels readings are separate constraints instantiating alternative interpretations of the same constitutional commitment. All three stories are linked via network.affects_constraints to enable corpus analysis of constraint families grounded in a single kernel. The ε-invariance principle requires separate constraint stories because the readings authorize different federal regulatory scopes, different victim sets, and different beneficiary structures—structurally distinct claims despite sharing a kernel. See commentary.kernel_context for the reading relations and differentiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
