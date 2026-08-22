% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Judicial Activism: Treaty Mandate Overreach
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The World Trade Organization's Dispute Settlement Body (DSB) was
 *   established in 1995 to interpret and enforce WTO agreements. This
 *   constraint story instantiates the JUDICIAL_ACTIVISM_READING of the
 *   contested kernel 'wto_dsb_authority.' Under this reading, DSB panels have
 *   systematically exceeded their mandate by creating new treaty obligations
 *   through interpretive expansion. Where the binding_referee_reading asserts
 *   member states delegated binding authority to panels, and the
 *   advisory_coordination_reading asserts panels provide non-binding
 *   guidance, the judicial_activism_reading argues panels have become
 *   illegitimate legislators, expanding treaty constraints beyond what member
 *   states agreed to. The reading expects active resistance to compliance,
 *   contestation of the interpretation itself, and member-state withdrawal
 *   from the enforcement mechanism. This story does NOT claim the panels are
 *   merely applying the treaty faithfully (that is the
 *   binding_referee_reading, a different constraint story). It claims the
 *   panels' interpretation IS the constraint, and that constraint is an act
 *   of judicial overreach grounded in no legitimate authority.
 *
 * KEY AGENTS:
 *   - DSB panel system (institutional agenda-setter): interprets treaties and issues binding rulings; under this reading, systematically extends treaty language beyond negotiated terms.
 *   - Policy-autonomous member states (powerful payers): discover new obligations from panel rulings they did not negotiate; resist through retaliation threats or withdrawal proposals.
 *   - Developing economies with policy-space constraints (moderate/powerless payers, identity-locked): cannot effectively contest panels, cannot exit, lose policy tools to interpretive expansion.
 *   - Large developed trading blocs (institutional beneficiaries): their economies absorb changes easily; they benefit from panels enforcing liberal rules against smaller competitors.
 *   - Treaty drafters and historical record (analytical observers, excluded): the original intent is read by panels in light of current incentives; their voice is absent from the mechanism.
 *   - Smaller member states (moderate, constrained, excluded): caught between principle that the DSB is neutral and reality that panels' rulings often work against them; cannot renegotiate effectively.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.71).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.68).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.77).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Judicial Activism: Treaty Mandate Overreach").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '18ff7128-1141-41c7-8f0e-cd0aab25ee92').
narrative_ontology:cs_kernel_codification('18ff7128-1141-41c7-8f0e-cd0aab25ee92', fixed_text).
narrative_ontology:cs_authority_grounding('18ff7128-1141-41c7-8f0e-cd0aab25ee92', extraction).
narrative_ontology:cs_interpretation_layer_present('18ff7128-1141-41c7-8f0e-cd0aab25ee92').
narrative_ontology:cs_reading_relation('18ff7128-1141-41c7-8f0e-cd0aab25ee92', wto_dsb_authority__binding_referee_reading, forecloses).
narrative_ontology:cs_reading_relation('18ff7128-1141-41c7-8f0e-cd0aab25ee92', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('18ff7128-1141-41c7-8f0e-cd0aab25ee92', foundational, treaty_boundaries_are_binding_on_interpretation).
narrative_ontology:cs_axiom_status(treaty_boundaries_are_binding_on_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('18ff7128-1141-41c7-8f0e-cd0aab25ee92', treaty_boundaries_are_binding_on_interpretation, deontological).
narrative_ontology:cs_axiom('18ff7128-1141-41c7-8f0e-cd0aab25ee92', foundational, member_state_consent_is_prerequisite_for_obligation).
narrative_ontology:cs_axiom_status(member_state_consent_is_prerequisite_for_obligation, holdable).
narrative_ontology:cs_axiom_grounding('18ff7128-1141-41c7-8f0e-cd0aab25ee92', member_state_consent_is_prerequisite_for_obligation, conventional).
narrative_ontology:cs_reference_frame('18ff7128-1141-41c7-8f0e-cd0aab25ee92', treaty_text_as_limiting_principle).
narrative_ontology:cs_drift_state('18ff7128-1141-41c7-8f0e-cd0aab25ee92', contemporary_expanded_panel_authority, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('18ff7128-1141-41c7-8f0e-cd0aab25ee92', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, policy_autonomous_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, developing_economies_with_policy_space_constraints).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, large_developed_trading_blocs).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dispute_complainants).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, judicial_overreach_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, treaty_sovereignty_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The DSB's Dispute Settlement panels interpret WTO agreements and issue binding rulings. They operate under formal mandate to resolve disputes consistent with the treaty's terms. This reading asserts that panels have systematically extended the treaty's language to create obligations not endorsed in the original text. Each decision becomes precedent, binding future panels. The system has no financial stake but institutional incentive to expand authority and demonstrate effectiveness.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_panel_system, agenda_setter,
    institutional, generational, analytical, global).

% Negotiated the original WTO agreements with defined language and understood scope. Discover through panel rulings that they have undertaken obligations beyond what they negotiated. Domestic constituencies bear adjustment costs. Exit is available (withdraw from DSB enforcement, threaten retaliation) but costs include loss of trade benefits and reciprocal retaliation against their exports. Increasingly contest the panels' authority and demand renegotiation of the treaty text itself.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, policy_autonomous_member_states, payer,
    powerful, generational, constrained, global).

% Lack resources to mount effective legal challenges in the DSB; their arguments are often overridden. Trade with developed economies is essential, so exit (withdrawal from the DSB) is economically catastrophic. Interpretive expansions frequently preclude policy tools they relied on for development (infant-industry protection, local content rules, price supports for staple crops). Their objections are heard in principle but not determinative.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, developing_economies_with_policy_space_constraints, payer,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, developing_economies_with_policy_space_constraints, excluded).

% Their large, diversified economies absorb DSB-mandated policy changes more easily. They often benefit from enforcement of liberal trading rules against smaller competitors. They can mount effective legal challenges, threaten retaliation credibly, and have leverage to shape panel arguments. Well-positioned within the interpretive discourse.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, large_developed_trading_blocs, beneficiary,
    institutional, generational, mobile, global).

% Use expansive panel interpretations to litigate trade barriers they otherwise could not challenge under narrower readings. Win favorable rulings that set precedent for future cases. Benefit from binding enforcement that gives panel decisions teeth and makes their victories durable.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dispute_complainants, beneficiary,
    powerful, biographical, mobile, global).

% Original negotiators and their immediate successors are not present in the mechanism. Their intentions are reconstructed by panels through the lens of current institutional incentives. This reading asserts panels selectively adopt interpretations of intent that favor expansive readings; the original negotiators cannot defend their actual understanding against reinterpretation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, treaty_drafters_and_negotiators, excluded,
    analytical, generational, analytical, global).

% Cannot credibly threaten retaliation; exit from DSB enforcement is not viable (trade with large partners is essential). Cannot effectively renegotiate treaty terms (consensus required for amendment, and large states block proposals that would constrain panels). Attempt to clarify treaty language are ineffective once panel precedent is established. Caught between formal principle that panels are neutral and practical reality that rulings often work against them.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, smaller_member_states, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__judicial_activism_reading, large_developed_trading_blocs).
narrative_ontology:fixing_cost_class(wto_dsb_authority__judicial_activism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolving trade disputes through a neutral, rule-based mechanism rather than unilateral retaliation or great-power bargaining. The reading contests whether this function is actually delivered when panels exceed the treaty's written terms and issue obligations member states did not agree to.
% TRANSFER_FUNCTION: Shifts effective policy authority from member-state governments (who negotiated the treaty) to appointed DSB panels (who interpret it after the fact). Moves policy flexibility and discretionary authority from elected representatives to adjudicators. Transfers compliance costs to member states whose domestic laws conflict with expansively interpreted panel rulings.
% ABSENT_VOICES: Non-state constituencies (labor, environmental, development advocates) who are not seated at the treaty negotiation table but bear consequences of panel interpretations. Nations that did not participate in a dispute are bound by the panel's precedent. Smaller trading nations with weaker legal resources and developing economies cannot mount effective counterarguments in the panel process and are structurally excluded from shaping interpretation.
% DISAPPEARANCE_RATIONALE: If the DSB's authority to bind member states on treaty interpretations exceeding the written text disappeared, the entire international trade system would reorganize. Member states would renegotiate WTO agreements with tighter language boundaries, establish a new dispute-resolution mechanism with explicit limits on interpretive authority, or revert to bilateral trade negotiations. Trade rules would revert from judge-made (panel interpretation) to negotiated (member consent).
% FOUNDING_PROBLEM: 1980s–1990s: member states needed a neutral, binding forum to resolve trade disputes without retaliation spirals. The GATT lacked binding dispute resolution; the DSB was created to supply it.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by trade historians and original negotiators' memoirs: yes, binding neutral dispute resolution was the goal. The reading asserts that panels have expanded beyond this founding problem into legislating new rules. This is contested by panels themselves (who claim they are merely clarifying the treaty's intent) and by developed trading blocs (who benefit from expansive readings). Developing economies, legal scholars critical of judicial overreach (e.g., Howse, Trachtman, Petersmann), and some developed nations' trade negotiators (esp. after the Appellate Body crisis, 2016–present) attest that panels have exceeded the original mandate and that member states are being bound by obligations they did not consent to.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.71) and rising over the 31-year interval (0.35 to 0.71) because the panels incrementally create binding obligations not explicitly stated in the treaty text. The rise reflects interpretive accretion: each panel decision extends the scope of what 'the treaty requires,' and member states discover they are bound by rules they did not explicitly endorse. This is the core claim of the reading: extraction is not from a bad bargain ex ante but from obligations imposed ex post through adjudication. Suppression is 0.68 because the DSB enforcement mechanism (retaliation authorization, precedent-binding) is what keeps member states in compliance with rules they view as illegitimate. Smaller states cannot exit without massive economic cost; larger states can threaten retaliation but face the cost of economic disruption. Theater_ratio rises from 0.18 to 0.42, reflecting that as resistance to compliance grows, the panels' framing of their role as 'applying the treaty' becomes increasingly performative—the 'legitimate interpretation' theater sustains compliance even as the underlying authority is contested. The measurement series is one shared grid, aligned on both time and metrics, capturing the trajectory of interpretive drift (extractiveness), suppression hardening (suppression_requirement), and theatrical maintenance (theater_ratio) over the DSB's history.
 *
 * PERSPECTIVAL GAP:
 *   The DSB panel system experiences this constraint as legitimate dispute resolution within their mandate. Policy-autonomous developed states experience it as acceptable enforcement of their preferred rules (or sometimes contestable but ultimately legitimate). Developing economies and smaller states experience it as illegitimate legislative overreach that they cannot escape. The engine computes these divergent classifications from the stakeholder-specific power, exit, and beneficiary/victim positions. The authored claim (snare: illegitimate judicial legislation) reflects the reading's own evaluative position; the measured extraction does not presume the reading's legitimacy judgment—it reports the observable fact that member states are bound by obligations they did not consent to.
 *
 * DIRECTIONALITY LOGIC:
 *   DSB panels (institutional, analytical, exit-less): directionality approaches zero — they are not targets of the constraint but its operators. However, they are not beneficiaries in the economic sense; they do not collect rents. Their interest is institutional authority persistence. Policy-autonomous member states (powerful, but constrained exit): directionality ~0.6–0.8. They negotiate the treaty, then discover new obligations from panels. Exit is costly (retaliation, exclusion from benefits), so they are partly trapped. Developing economies (moderate-to-powerless, identity-locked): directionality ~0.85. They lose policy tools, cannot contest effectively, cannot exit. Developed trading blocs (institutional, mobile, arbitrage exit): directionality ~0.2–0.3 (beneficiaries). They can threaten retaliation credibly and often benefit from liberal rulings. The structural asymmetry: the same panel ruling is experienced as legitimate enforcement by beneficiaries and illegitimate overreach by targets. The derived directionalities capture this: beneficiaries sit near 0 (subsidy/benefit), targets sit near 1 (extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (neutral dispute resolution in lieu of retaliation spirals) was live at 1995. By 2026, the reading asserts the founding problem is partially dead and partially contested. The panels' interpretive function still provides neutral adjudication, but the assertion of new obligations makes the mechanism asymmetrically beneficial to large trading blocs. The reading classifies the constraint as SNARE, not tangled_rope, because it asserts the coordination function is cover for judicial legislation: the 'neutral forum' theater sustains compliance by developing nations even as the substantive rules are illegitimate. This mandatrophy pattern (founding function atrophies, new extractive function grows) is exactly why the rising theater_ratio matters: if the coordinate function were the primary mechanism, theater would be low (the real work is getting done). Instead, theater rises as resistance mounts, indicating the legitimacy story ('we are just applying the treaty') becomes increasingly necessary to sustain compliance. The measurement trajectory supports the mandatrophy reading: extractiveness accumulates, suppression hardens, and theater rises to maintain the facade of legitimate interpretation against growing resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_mandate_boundary,
    'Where is the boundary between faithful treaty interpretation and legislative overreach? Did specific DSB rulings (e.g., Appellate Body decisions on WTO Article XX exceptions, TRIPS enforcement, safeguards) exceed the treaty text or legitimately clarify ambiguities the drafters left intentionally open?',
    'Comparative analysis of treaty language vs. panel holdings; testimony from original drafters and negotiators; linguistic and historical reconstruction of intent; examination of preparatory works (travaux préparatoires).',
    'If panels exceed the text, the constraint is snare (illegitimate judicial imposition). If they clarify intended ambiguity, the constraint is tangled_rope (coordination with asymmetric authority). The reading''s entire classification hinges on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_mandate_boundary, conceptual, 'The structural boundary between legitimate interpretation and legislative overreach.').

omega_variable(
    procedural_legitimacy_vs_substantive_authority,
    'Is the loss of legitimacy from interpretive drift offset by the procedural legitimacy of the DSB as a neutral forum? Do member states accept unfavorable rulings because the forum is legitimate even if specific outcomes exceed mandate, or do they resist on the grounds that the forum itself has become illegitimate?',
    'Analysis of compliance rates over time, state statements in WTO bodies, negotiation of DSB reform proposals, withdrawal threats, and unilateral defections from DSB authority. Track the shift from ''we lost a legitimate case'' to ''the system itself is illegitimate.''',
    'High procedural legitimacy can sustain compliance even with substantive overreach (tangled_rope tipping point). Low procedural legitimacy triggers withdrawal and retaliation (snare acceleration). This determines whether the constraint persists through consent or coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_legitimacy_vs_substantive_authority, empirical, 'Whether procedural legitimacy sustains compliance despite substantive authority drift.').

omega_variable(
    sibling_reading_framing_dependence,
    'This reading asserts judicial activism and illegitimate legislation. The binding_referee_reading asserts lawful delegation of binding authority. Does the same panel decision count as judicial overreach or legitimate enforcement depending on which reading frames the observation?',
    'The ε-invariance test: if changing the reading changes what the panels do (the observable), then the constraint has shifted. If the panels'' structural behavior stays constant but the reading''s normative judgment of it changes, then two readings share one constraint referent. This is the framing-dependence omega for kernel readings.',
    'If the readings genuinely alter the observable constraint (different ε values from different interpretations of what ''mandate'' means), the readings are distinct constraints (two files, network link). If they share the referent and differ only in normative frame, then ε should be reading-indexed but stable, and the frames are committer positions on one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_framing_dependence, conceptual, 'Whether sibling readings describe the same constraint or different ones; whether framing dependence means we have one constraint with multiple readings or multiple constraints.').

omega_variable(
    exit_option_asymmetry_by_power,
    'Large developed economies have more effective exit options (can credibly threaten retaliation, can forum-shop, can negotiate bilaterally). Smaller and developing economies are trapped. Does this asymmetry constitute suppression of the constraint, or is it a structural feature that makes the constraint extractive regardless of the panels'' intent?',
    'Compare compliance rates and resistance levels by member state power. Track whether smaller states openly contest DSB authority vs. accepting unfavorable rulings in silence. Examine renegotiation or withdrawal proposals by power tier.',
    'If suppression is structural (power-based exit inequality), then the constraint is snare regardless of panel legitimacy. If suppression is active enforcement by the DSB against non-compliance, then the classification depends on whether the enforcement is legitimate. The reading asserts the panels'' authority is illegitimate, so active enforcement backing illegitimate mandates is coercive suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_asymmetry_by_power, empirical, 'Whether measured suppression is structural inequality or active DSB enforcement of illegitimate mandates.').

omega_variable(
    kernel_reading_legitimacy_source,
    'This reading (judicial_activism_reading) asserts that panels lack legitimacy to extend the treaty through interpretation. The binding_referee_reading asserts they have legitimate delegated authority. What GROUNDS the legitimacy claim in each reading—consent, expertise, institutional track record, treaty text, or something else? Does the reading''s assertion of illegitimacy depend on contesting the SOURCE of legitimacy or the APPLICATION of legitimate authority?',
    'Deconstruct the reading''s own grounding: if it asserts ''panels lack consent from member states for expansive interpretation,'' the ground is consent-based legitimacy. If it asserts ''panels are biased toward developed economies,'' the ground is impartiality-based legitimacy. If it asserts ''panels misread the treaty text,'' the ground is fidelity-based legitimacy. The sibling readings will ground legitimacy differently. This omega surfaces the axiom distinction for cs_structure.axioms.',
    'The axioms in cs_structure reflect this: this reading''s foundational axiom is likely ''treaty_boundaries_are_binding_on_interpretation'' (panels must not extend beyond text). The binding_referee reading''s axiom is likely ''delegated_interpretive_authority_is_legitimate'' (member states did consent to panels clarifying ambiguities). These are the competing normative premises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy_source, conceptual, 'The legitimacy grounds this reading rests on and how they differ from sibling readings'' grounds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement_basis(wto__tr_t1995, observed).
narrative_ontology:measurement(wto__tr_t2001, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2001, 0.24).
narrative_ontology:measurement_basis(wto__tr_t2001, observed).
narrative_ontology:measurement(wto__tr_t2008, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2008, 0.31).
narrative_ontology:measurement_basis(wto__tr_t2008, observed).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(wto__tr_t2015, observed).
narrative_ontology:measurement(wto__tr_t2020, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(wto__tr_t2020, observed).
narrative_ontology:measurement(wto__tr_t2026, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(wto__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement_basis(wto__be_t1995, observed).
narrative_ontology:measurement(wto__be_t2001, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2001, 0.48).
narrative_ontology:measurement_basis(wto__be_t2001, observed).
narrative_ontology:measurement(wto__be_t2008, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2008, 0.59).
narrative_ontology:measurement_basis(wto__be_t2008, observed).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement_basis(wto__be_t2015, observed).
narrative_ontology:measurement(wto__be_t2020, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2020, 0.69).
narrative_ontology:measurement_basis(wto__be_t2020, observed).
narrative_ontology:measurement(wto__be_t2026, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2026, 0.71).
narrative_ontology:measurement_basis(wto__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement_basis(wto__su_t1995, observed).
narrative_ontology:measurement(wto__su_t2001, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2001, 0.52).
narrative_ontology:measurement_basis(wto__su_t2001, observed).
narrative_ontology:measurement(wto__su_t2008, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2008, 0.59).
narrative_ontology:measurement_basis(wto__su_t2008, observed).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement_basis(wto__su_t2015, observed).
narrative_ontology:measurement(wto__su_t2020, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2020, 0.66).
narrative_ontology:measurement_basis(wto__su_t2020, observed).
narrative_ontology:measurement(wto__su_t2026, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2026, 0.68).
narrative_ontology:measurement_basis(wto__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__judicial_activism_reading, 0.12).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, us_withdrawal_from_wto_enforcement).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, developing_economy_trade_negotiation_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel wto_dsb_authority. The binding_referee_reading models the DSB as having legitimate delegated authority (tangled_rope: coordination + enforcement). The advisory_coordination_reading models the DSB as providing non-binding guidance (rope: genuine coordination). The judicial_activism_reading (this story) models the DSB as exceeding its mandate through interpretive expansion (snare: illegitimate legislation). All three stories share the referent (the WTO treaty and DSB structure) but instantiate different ε values and classifications based on reading-specific interpretations of what the treaty grants and what panels actually do. The three readings coexist: different member states, different legal traditions, and different institutional positions produce different readings of the same kernel. Link the three constraint files via network.affects_constraints to enable contamination analysis: if one reading's authority erodes (e.g., judicial_activism_reading gains acceptance and member states withdraw), the sibling readings' structural premises weaken.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
