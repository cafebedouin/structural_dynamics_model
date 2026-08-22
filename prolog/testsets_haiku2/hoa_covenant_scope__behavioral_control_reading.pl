% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant Behavioral Control Reading
 *   domain: property_law/collective_governance
 *
 * SUMMARY:
 *   HOA covenants are enforced in the United States by homeowner associations
 *   that interpret vague aesthetic and behavioral language to police
 *   neighborhood conformity. This reading instantiates the covenant as a
 *   behavioral-control mechanism: a snare that extracts compliance with
 *   majority-aligned aesthetics and lifestyle norms from nonconformists and
 *   marginal residents, suppresses visible diversity, and operates under
 *   cover of property-value protection. The reading contests the
 *   'coordination' framing (shared infrastructure, externality management)
 *   and the 'extraction' framing (pure rent-seeking via fine proliferation)
 *   by focusing on the covenant's function as a mechanism for enforcing
 *   social homogeneity and suppressing behavioral and aesthetic deviation.
 *   This is one reading of a contested kernel: the same covenant text is read
 *   by different communities as either coordination (keep the neighborhood
 *   maintained), extraction (board power consolidation), or behavioral
 *   control (suppress nonconformity). The claim and metrics are independent:
 *   the constraint is CLAIMED as snare; the metrics describe moderate-high
 *   suppression, rising extraction, and increasing theater (performative
 *   enforcement of aesthetic rather than functional standards).
 *
 * KEY AGENTS:
 *   - Board-aligned homeowners: Interpret and enforce covenant language; benefit from control authority; form majority coalition that sustains enforcement
 *   - Nonconformist homeowners: Bear fines and remediation costs; face selective enforcement; trapped by high exit costs; structurally powerless
 *   - Marginal-aesthetic households: Face disproportionate enforcement due to implicit class/cultural aesthetic bias; identity-locked to properties they cannot afford to modify
 *   - Speech-restricted residents: Covenant vagueness enables selective suppression of political and religious signage; speech control operates as behavioral control proxy
 *   - Conformist majority: Benefit from uniformity without bearing enforcement costs; perceive property value protection from strict policing
 *   - Prospective buyers: Excluded from enforcement reality at purchase time; discover intensity only after commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.48).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.71).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant Behavioral Control Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '737c50da-bbd0-465b-94fe-72ee113ec904').
narrative_ontology:cs_kernel_codification('737c50da-bbd0-465b-94fe-72ee113ec904', fixed_text).
narrative_ontology:cs_authority_grounding('737c50da-bbd0-465b-94fe-72ee113ec904', extraction).
narrative_ontology:cs_interpretation_layer_present('737c50da-bbd0-465b-94fe-72ee113ec904').
narrative_ontology:cs_reading_relation('737c50da-bbd0-465b-94fe-72ee113ec904', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('737c50da-bbd0-465b-94fe-72ee113ec904', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('737c50da-bbd0-465b-94fe-72ee113ec904', foundational, behavioral_conformity_is_extractive).
narrative_ontology:cs_axiom_status(behavioral_conformity_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('737c50da-bbd0-465b-94fe-72ee113ec904', behavioral_conformity_is_extractive, deontological).
narrative_ontology:cs_axiom('737c50da-bbd0-465b-94fe-72ee113ec904', foundational, aesthetic_uniformity_enforces_social_homogeneity).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_enforces_social_homogeneity, holdable).
narrative_ontology:cs_axiom_grounding('737c50da-bbd0-465b-94fe-72ee113ec904', aesthetic_uniformity_enforces_social_homogeneity, empirically_contingent).
narrative_ontology:cs_reference_frame('737c50da-bbd0-465b-94fe-72ee113ec904', majority_choice_aesthetic_governance).
narrative_ontology:cs_drift_state('737c50da-bbd0-465b-94fe-72ee113ec904', contemporary_diversity_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('737c50da-bbd0-465b-94fe-72ee113ec904', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetic_households).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, speech_restricted_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Homeowners whose aesthetic preferences and lifestyle choices align with the covenant's standards. They benefit from the enforcement of uniformity without bearing enforcement costs directly, as their properties comply naturally. They perceive property value protection from strict conformity policing and form the coalition that sustains board enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners, beneficiary,
    moderate, biographical, constrained, local).

% Board members and their allied homeowners set covenant enforcement priorities, interpret vague aesthetic standards, and direct fines and remediation orders. They collect enforcement authority and determine whose properties trigger scrutiny. They benefit from the power to shape neighborhood character and from the deference of conforming majority.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, agenda_setter,
    powerful, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary).

% Homeowners whose aesthetic choices or lifestyle expressions deviate from covenant standards: unconventional landscaping, visible repairs, non-traditional exterior color, religious or political signage, or simply aging properties. They face selective enforcement, fines, remediation demands, and reputational targeting. Exit means selling at discount (buyer assumes covenant risk) or abandoning the property.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    powerless, biographical, trapped, local).

% Households whose aesthetic markers (cultural exterior decoration, multi-generational family gatherings, visible disability accommodations, or economic markers like weathered furnishings) fall outside the covenant's implicit class/cultural aesthetic. They face disproportionate enforcement because aesthetic judgment operates as proxy for social control. Many cannot afford to relocate or to comply with expensive remediation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetic_households, payer,
    powerless, biographical, identity_locked, local).

% Residents who want to display political, religious, or solidarity signage (yard signs, flags, banners) but are blocked by covenant interpretation of 'aesthetic standards.' The covenant's vagueness allows selective enforcement: permitted signs for conformist causes, forbidden signs for marginal political positions. Speech suppression operates as de facto behavioral control.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, speech_restricted_residents, payer,
    powerless, biographical, trapped, local).

% The formal governing structure that interprets and enforces covenant language. The board controls which properties get inspected, which violations get enforced, which residents get fined, and how much remediation must cost. The board can defer enforcement selectively or escalate it strategically.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    institutional, biographical, mobile, local).

% The codified rules and recorded language of the covenant runs. The text is rarely specific: 'aesthetic harmony,' 'in keeping with neighborhood character,' 'not detrimental to property values,' 'reasonable restrictions on use.' This vagueness is the structural enabler of behavioral control — the text does not constrain interpretation, it invites it.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, covenant_text, beneficiary,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(hoa_covenant_scope__behavioral_control_reading, covenant_text).

% Potential residents are told a covenant exists and shown recorded language, but the lived enforcement reality — which properties get targeted, how aggressively fines escalate, how far the board interprets aesthetic standards — is rarely transparent. Buyers discover enforcement intensity only after purchase, at which point exit costs are catastrophic.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, prospective_buyers, excluded,
    moderate, biographical, constrained, local).

% Prospective residents whose aesthetic or lifestyle preferences are known to deviate (multicultural households, disability-visible families, politically active residents) may be deterred from purchasing or may not be shown properties in covenant communities. The covenant's enforcement signals a narrowed acceptable population, excluding diversity preemptively.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_buyers_excluded, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__behavioral_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enforces uniformity in property appearance and resident behavior as a claim about shared property value — coordinates resident expectations that neighbors' choices will not diminish resale value through aesthetic deviation or visible social difference.
% TRANSFER_FUNCTION: Moves aesthetic and behavioral control authority from individual property owners to the board; moves compliance costs (remediation, fines, legal defense) from the conformist majority to nonconformist payers; moves social legitimacy (whose aesthetic is default, whose is deviance) from the marginal households to the majority-aligned households.
% ABSENT_VOICES: Prospective nonconformist buyers are excluded from the neighborhood before purchase. Renters (many in covenant communities) have no vote in covenant enforcement. Long-term residents who age in place and gradually fall out of conformity (weathered properties, disability-visible modifications) are structurally unable to contest enforcement escalation.
% DISAPPEARANCE_RATIONALE: If covenant enforcement disappeared, nonconformist and marginal households would cease paying fines and complying with remediation, visible diversity of aesthetic and cultural expression would increase sharply, and the board's authority to police resident behavior would collapse. The neighborhood appearance would become less uniform; property resale markets would reflect the constraint's removal in price negotiation and buyer composition.
% FOUNDING_PROBLEM: Early covenant language invoked 'protection of property values' through aesthetic control. The framing held that uniform, tidy neighborhoods commanded resale premiums and that individual owner choices threatened collective value — a genuine market claim in segregated mid-century real estate.
% FOUNDING_PROBLEM_CORROBORATION: Real-estate economics research shows aesthetics DO influence property values, but the research is agnostic on whether strict uniformity maximizes value or whether diversity and mixed aesthetics correlate with value in contemporary markets. Board members and conformist homeowners attest the founding problem is live. Nonconformist residents and academic real-estate researchers attest the market has shifted and uniformity enforcement is now a proxy for social homogeneity control divorced from actual value dynamics.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the constraint moves compliance costs and aesthetic control authority, but does not generate direct financial revenue like fine proliferation (extraction reading) or solve major collective action problems (coordination reading). Suppression is high (0.71) because nonconformity faces active, escalating enforcement and payers have few exits: selling means accepting covenant-risk discount, staying means compliance. Theater rises from 0.25 to 0.42 over the interval because enforcement increasingly targets aesthetic appearance (yard signs, exterior colors, aging surfaces) rather than actual externalities (noise, safety, shared infrastructure). The measurement series tracks the constraint's drift from ostensible property-value protection toward behavioral and aesthetic control, with suppression rising faster than extractiveness — the constraint becomes more coercive before it becomes more extractive, a signature of snare emergence. All metrics are authored on one shared temporal grid: every metric carries every time point in the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (board-aligned, powerful, mobile exit) experiences the covenant as a legitimate governance mechanism and property protector; the constraint should compute as rope or low-grade tangled-rope from that seat. The payer seats (nonconformist, marginal, powerless, trapped) experience the covenant as suppressive and extractive; the constraint should compute as snare from those seats. The conformist-majority seat experiences genuine coordination benefit (property value protection) without visible extraction cost (natural compliance); the constraint should compute as rope from that seat. These three divergent computations ARE the measurement this reading enables — they arise directly from the structural data (different power atoms, different exit options, different role relationships) without the claim needing to adjudicate them. The reading's claim as snare focuses interpretation on the payer seats and highlights the suppression and behavioral-control mechanisms; the engine computes what each seat actually experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading's core premise is that the covenant function is behavioral and aesthetic control, not coordination or revenue extraction. From the board-aligned seat (institutional power, mobile exit, beneficiary role), the covenant is a legitimate expression of majority preference and property value protection — low directionality toward extraction, high directionality toward coordination benefit. From the nonconformist seat (powerless, trapped exit, payer role), the covenant is a mechanism of suppression and social homogeneity enforcement — high directionality toward extraction target. From the conformist majority seat (moderate power, constrained exit, beneficiary role), the covenant is collectively protective without visible cost (they comply naturally) — low directionality toward extraction. The engine computes these per-seat divergences from the structural data (power, exit, role); this reading's claim does not resolve them. The reading's strength is that it makes the divergence itself the analytical object: who benefits (majority, board) and who bears costs (nonconformists, marginal aesthetics) is exactly the axis this reading's frame highlights, whereas coordination reading minimizes it and extraction reading channels it through financial mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('property value protection through uniformity') is contested because contemporary real-estate research shows aesthetic diversity does not uniformly damage values and may enhance them in mixed-use communities. The board and conformist majority assert the problem is live and enforcement is necessary. Nonconformist residents and academic researchers assert the founding problem is dead — property markets have shifted and enforcement now persists as social homogeneity control divorced from value dynamics. The snare classification requires both extraction (the constraint moves costs to payers) and suppression (payers have constrained exits). The behavioral-control reading emphasizes suppression over revenue extraction: the board's power and the conformist majority's control over neighborhood character are extracted, not primarily money. This distinguishes the reading from the 'extraction' reading (which centers fine proliferation and financial revenue). The reading avoids false-rope classification by naming suppression as the primary enforcement mechanism and behavioral/aesthetic conformity as the primary transfer function, not just property value or coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_value_vs_social_conformity,
    'Is the covenant''s enforcement genuinely driven by property-value protection, or is property-value protection a cover story for social conformity and homogeneity enforcement?',
    'Comparative analysis: do covenants with relaxed aesthetic enforcement (but maintained functional standards for infrastructure and safety) show measurable value degradation relative to strict aesthetic-control covenants in similar markets? Do neighborhoods with diverse aesthetic markers maintain stable property values?',
    'If property values are robust under relaxed aesthetic enforcement, the reading''s core premise (behavioral control, not value protection) is strongly supported and the constraint should classify as pure snare. If aesthetic uniformity is empirically necessary for value, the classification becomes more ambiguous (snare with genuine value-protection function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_value_vs_social_conformity, empirical, 'Whether the founding problem (property value maximization via aesthetic uniformity) is real or a cover story for behavioral control.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) primarily structural (legal consequences, enforcement machinery) or internalized (residents have incorporated conformity into self-concept and identity)?',
    'Post-exit interviews with former residents: do they report that suppression persists after leaving the covenant community (internalized), or does behavioral and aesthetic freedom resume immediately after exit (structural)?',
    'If suppression is primarily structural, the constraint''s effective suppression is the authored 0.71 and can potentially be dismantled by removing enforcement. If substantially internalized, residents carry the suppression with them and the constraint''s true reach exceeds the mechanical measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in covenant behavioral control.').

omega_variable(
    majority_preference_vs_minority_suppression,
    'Is the conformist majority''s preference for aesthetic uniformity a genuine collective choice, or is it itself produced by covenant enforcement and social signaling that has made deviation costly?',
    'Counterfactual analysis: remove enforcement and observe whether preference for uniformity persists or whether aesthetic diversity becomes the stable equilibrium within 3-5 years.',
    'If majority preference is endogenous to enforcement (produced BY the constraint), the constraint is not expressing pre-existing consensus but rather creating and sustaining it through suppression. The reading''s snare classification is reinforced — the constraint manufactures the majority it claims to serve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majority_preference_vs_minority_suppression, conceptual, 'Whether conformist majority preference is exogenous or endogenous to covenant enforcement.').

omega_variable(
    reading_foreclosure_test,
    'Do the three readings (coordination, extraction, behavioral_control) logically foreclose each other, or do they coexist as different frames applied to the same kernel by different constituencies?',
    'Structural analysis: does the behavioral_control reading''s core claim (enforcement of social conformity) logically rule out the coordination reading''s core claim (genuine collective-action solution)? Or can a single covenant both solve coordination problems AND enforce behavioral control?',
    'If readings coexist (can both be true simultaneously), the kernel exhibits genuine interpretive under-determination and the three constraint stories form a family linked by network edges. If one reading forecloses another, the engine''s signature detection should identify the foreclosing relation and the false reading as a cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Logical independence or foreclosure among the three readings of the hoa_covenant_scope kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(hoa__tr_t0, observed).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(hoa__tr_t5, observed).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(hoa__tr_t10, observed).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(hoa__tr_t15, observed).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(hoa__tr_t20, observed).
narrative_ontology:measurement(hoa__tr_t25, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(hoa__tr_t25, observed).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(hoa__tr_t30, observed).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(hoa__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(hoa__be_t5, observed).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement_basis(hoa__be_t10, observed).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement_basis(hoa__be_t15, observed).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(hoa__be_t20, observed).
narrative_ontology:measurement(hoa__be_t25, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 25, 0.46).
narrative_ontology:measurement_basis(hoa__be_t25, observed).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(hoa__be_t30, observed).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(hoa__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(hoa__su_t0, observed).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(hoa__su_t5, observed).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(hoa__su_t10, observed).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(hoa__su_t15, observed).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(hoa__su_t20, observed).
narrative_ontology:measurement(hoa__su_t25, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(hoa__su_t25, observed).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(hoa__su_t30, observed).
narrative_ontology:measurement(hoa__su_t40, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(hoa__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__behavioral_control_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The hoa_covenant_scope kernel decomposes into three distinct constraint stories, each with different ε, different beneficiary/victim structures, and different types. The behavioral_control_reading (this story) focuses on the enforcement of social conformity and aesthetic uniformity as mechanisms of suppression; the coordination_reading emphasizes genuine collective-action functions (infrastructure, shared amenities); the extraction_reading emphasizes revenue and power-consolidation through fine proliferation. All three readings share the same recorded covenant text and enforcement structure (the kernel), but interpret its function, beneficiaries, and mode of operation differently. Network links express the constraint family structure — changes in one reading's classification or corroboration affect the others' standing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__behavioral_control_reading, powerless, 0.88).
constraint_indexing:directionality_override(hoa_covenant_scope__behavioral_control_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
