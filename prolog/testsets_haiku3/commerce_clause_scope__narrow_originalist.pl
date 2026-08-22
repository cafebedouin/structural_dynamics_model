% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Interstate Commerce Facilitation (Narrow Originalist Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   The narrow originalist reading of the Commerce Clause interprets
 *   'commerce among the states' as literally meaning trade crossing state
 *   lines, and 'regulate' as make regular (remove barriers and ensure uniform
 *   rules) not restrict or comprehensively control. This reading limits
 *   federal legislative power to (1) removing state-imposed barriers to
 *   interstate transactions and (2) setting uniform rules for genuinely
 *   interstate commerce. Intrastate economic activity, local labor standards,
 *   civil rights in non-commercial contexts, and environmental protection of
 *   purely local resources remain under state police power. This is a
 *   self-conscious limitation doctrine, advanced by originalist scholars and
 *   embodied in decisions like United States v. Lopez (1995) and United
 *   States v. Morrison (2000). The constraint's extractiveness is moderate
 *   (0.42 at present): it privileges state autonomy and local business over
 *   federal regulatory reach, but is constrained by the coordination function
 *   (removing state barriers) which benefits interstate commerce. The
 *   claim/metric gap is intentional: originalists frame the reading as a
 *   neutral discovery of constitutional text, but the metrics describe an
 *   interpretive choice that extracts from federal authority and national
 *   uniformity in favor of state decentralization.
 *
 * KEY AGENTS:
 *   - State governments — primary beneficiaries, retain police power over intrastate activity
 *   - Local businesses — benefit from freedom from federal mandates while facing state-set rules
 *   - Federal courts — agenda-setter, interpreting and enforcing the originalist reading
 *   - Civil rights claimants in recalcitrant states — primary victims, lose federal enforcement authority
 *   - Environmental protection advocates — lose federal authority over intrastate pollution
 *   - Interstate commerce operators — benefit from removal of state barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.42).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.28).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.42).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Interstate Commerce Facilitation (Narrow Originalist Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional_law/federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, 'fc7829d0-e2c0-45cd-9e4d-6580b9ed620d').
narrative_ontology:cs_kernel_codification('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', fixed_text).
narrative_ontology:cs_authority_grounding('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', lineage).
narrative_ontology:cs_interpretation_layer_present('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d').
narrative_ontology:cs_reading_relation('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_reading_relation('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', foundational, commerce_means_trade_crossing_state_lines).
narrative_ontology:cs_axiom_status(commerce_means_trade_crossing_state_lines, holdable).
narrative_ontology:cs_axiom_grounding('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', commerce_means_trade_crossing_state_lines, empirically_contingent).
narrative_ontology:cs_axiom('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', foundational, regulate_means_make_regular_not_control).
narrative_ontology:cs_axiom_status(regulate_means_make_regular_not_control, holdable).
narrative_ontology:cs_axiom_grounding('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', regulate_means_make_regular_not_control, empirically_contingent).
narrative_ontology:cs_axiom('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', secondary, federalism_preserves_state_autonomy_over_intrastate_activity).
narrative_ontology:cs_axiom_status(federalism_preserves_state_autonomy_over_intrastate_activity, holdable).
narrative_ontology:cs_axiom_grounding('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', federalism_preserves_state_autonomy_over_intrastate_activity, deontological).
narrative_ontology:cs_reference_frame('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', framers_commerce_among_states_as_interstate_trade).
narrative_ontology:cs_drift_state('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', contemporary_2026, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fc7829d0-e2c0-45cd-9e4d-6580b9ed620d', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_enforcement_in_recalcitrant_states).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_environmental_protection).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, interstate_commerce_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain broad police powers over intrastate economic activity, labor standards, environmental protection, and social regulation. Under this reading, their authority to set local commercial norms is constitutionally protected from federal preemption unless the activity directly crosses state lines. They benefit from reduced federal constraint on their regulatory autonomy and the ability to experiment with policies suited to local conditions.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, analytical, national).

% Face regulation primarily by state and local governments whose rules they can influence through proximity and political engagement. Interstate commerce barriers are removed, but purely local operations remain subject only to state/local oversight. They benefit from a predictable, locally-calibrated regulatory environment and reduced exposure to federal mandates.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    organized, biographical, constrained, regional).

% The principle that states serve as laboratories for policy innovation. Under narrow originalism, this is vindicated: states are free to experiment with labor standards, environmental approaches, consumer protections, and social policy without federal interference in intrastate matters. The constraint protects this function by limiting federal reach to interstate channels.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, decentralized_experimentation, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, decentralized_experimentation).

% The principle that nationwide regulatory consistency serves interstate commerce efficiency and protects citizens uniformly across the country. Under narrow originalism, this principle is constrained: federal authority to mandate uniform labor standards, environmental rules, civil rights protections across all states is substantially limited. The constraint extracts from this principle by privileging state autonomy over national uniformity.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).

% Citizens in states resistant to civil rights protections (voting rights, employment discrimination, housing access) depend on federal enforcement to override state-level refusal. Under narrow originalism, federal civil rights laws that regulate non-commercial activity (e.g., public accommodations not directly engaged in interstate commerce, voting procedures in state elections) are constitutionally vulnerable. These citizens bear the cost of reduced federal reach.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_enforcement_in_recalcitrant_states, payer,
    powerless, biographical, trapped, local).

% The regulatory capacity to enforce clean air, water, and climate standards nationwide. Under narrow originalism, environmental regulation of intrastate activities that do not directly cross state lines (e.g., factory emissions that accumulate locally, wetlands not connected to navigable waters) falls outside federal commerce power. The constraint extracts from this principle by requiring direct interstate commercial impact for federal authority.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_environmental_protection, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, federal_environmental_protection).

% Interpret and enforce the Constitution's text and structure. Under this reading, they apply originalist principles to discern the Framers' understanding of 'commerce among the states' as limited to trade crossing state lines. They set doctrine by invalidating federal statutes they find to exceed this scope and upholding state laws within their retained sphere.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Enterprises engaged in genuinely interstate transactions benefit from federal removal of state-imposed barriers (tariffs, discriminatory rules) that fragment the national market. This reading guarantees their right to move goods across state lines without state obstruction while preserving their exposure only to federal uniform rules, not to a patchwork of state mandates.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, interstate_commerce_operators, beneficiary,
    powerful, biographical, mobile, national).

% Has enacted civil rights, environmental, labor, and social legislation that exercises federal commerce power. Under narrow originalism, the constitutional scope of that power is substantially narrowed; Congress's authority to regulate non-commercial or purely intrastate activity via the Commerce Clause is constitutionally constrained, limiting the legislative toolkit for nationwide policy.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, congress, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__narrow_originalist, diffuse).
narrative_ontology:fixing_cost_class(commerce_clause_scope__narrow_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes state-imposed barriers to interstate commerce (tariffs, discriminatory licensing, differential tax treatment) so goods and services can move across state lines freely and predictably; establishes that federal government may set uniform rules for genuinely interstate commercial transactions so merchants face one national standard rather than fifty different state standards.
% TRANSFER_FUNCTION: Transfers regulatory authority from individual states to the federal level only for activities that directly constitute interstate commerce (goods crossing state lines, interstate transactions). Leaves intrastate economic activity, local labor standards, environmental protection, and social regulation under state control. The constraint privileges state police power over federal reach in local matters.
% ABSENT_VOICES: Beneficiaries of federal civil rights enforcement (minority citizens in resistant states), environmental advocates seeking federal pollution control, labor advocates seeking minimum nationwide standards, and interstate commerce participants harmed by state-level discrimination are not present to argue for broader federal power. Their cases and grievances appear in litigation but lack a formal seat at the constitutional interpretation table where originalist premises are debated.
% DISAPPEARANCE_RATIONALE: If this reading's constitutional constraint disappeared — if courts adopted broad-effects doctrine instead — the world would rearrange substantially: Congress could regulate any intrastate economic activity with aggregate interstate effects (agriculture, manufacturing, service provision), civil rights enforcement would expand nationwide without requiring nexus to interstate commerce, federal environmental authority would extend to purely local pollution, and state regulatory autonomy would contract. If the reading were replaced by intermediate-channels doctrine, rearrangement would be partial: some federal reach would expand, but with limiting principles. The disappearance verdict is contested because the reading's supporters argue the Constitution itself creates this limit (natural law, discovered not made), while opponents argue the constraint is a constructed political preference vulnerable to reinterpretation.
% FOUNDING_PROBLEM: The Framers established a federal system in which the national government could regulate commerce among the states (removing state-imposed barriers, setting uniform rules for interstate transactions) without assuming police power over all economic activity. The problem was preventing balkanization of the national market by state tariffs and discrimination while preserving state autonomy over local affairs.
% FOUNDING_PROBLEM_CORROBORATION: Originalist constitutional scholars (Randy Barnett, Ilya Somin, and others in the Cato Institute and Federalist Society) argue the founding problem is live and the original understanding of the Commerce Clause reflects this limited scope. The Federal government (via Solicitor General arguments in Lopez, Morrison, and NFIB v. Sebelius) argues the founding problem is substantially transformed by modern economic interdependence and the problem now is coordinating nationwide regulation. Broad-construction scholars (Tribe, Sunstein, Ackerman) argue the Framers' original problem has been superseded by the administrative state's need for comprehensive economic regulation. No authoritative source outside the constitutional tradition itself speaks to what the problem 'really was' — the testimony is entirely from the reading's own tradition and its critics within that tradition.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, contested).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42) rather than low because the reading privileges state autonomy at the cost of federal regulatory reach, which extracts from national uniformity and civil rights protection. However, it is not high (would require suppression or coercion) because the mechanism is textual interpretation, not force; participants accept the constraint as legitimate constitutional law even when they disagree with it. Suppression is low (0.28) because the reading does not require active suppression of alternatives — it operates through judicial doctrine and separation of powers, not coercion. Theater is very low (0.12) because the genuine function (removing state barriers) dominates; there is little performative activity. Accessibility collapse is high (0.68) because once the originalist reading is accepted, the alternative (broad-effects doctrine) seems categorically unavailable within a textualist frame — the reading presents itself as the only correct reading of the text, collapsing alternatives into misinterpretation. Resistance is high (0.74) because the reading meets substantial opposition: decades of broad-construction precedent, legislative testimony from agencies defending their authority, public interest groups defending federal civil rights enforcement, and academic consensus supporting broad doctrine all resist the reading's narrowing implications. The measurement series tracks the reading's historical trajectory: low extractiveness at the Founding (the problem is genuinely about removing barriers), rising through the 19th century (federal power expands while states retain de facto autonomy, creating tension), peaking at 1937 (the New Deal crisis and its resolution), then declining from 1995 onward as originalism gained intellectual ground but faced institutional resistance. Theater rises slightly through the 20th century (courts had to justify broad doctrine in originalist terms, creating performative gap) before declining as originalism became more openly acknowledged as an interpretive choice rather than a mechanical discovery.
 *
 * PERSPECTIVAL GAP:
 *   The state government seat and the federal civil rights enforcement seat compute very differently. From the state government perspective, the constraint is a legitimate protection of federalism and local autonomy — a rope coordinating the division of powers. From the civil rights victim seat (African American voters in states with resistance to voting rights, employers in states with weak civil rights laws), the same structure operates as a snare: federal power to override state discrimination is withdrawn, and the victim is trapped in a state-level regulatory environment hostile to their rights. The engine computes these divergent classifications from the structural data: state governments have high power and mobility (they can redesign their rules within their sphere, move judicial officers, engage in bargaining), so low directionality; civil rights claimants have low power, trapped exit (cannot exit the state, cannot appeal to federal authority under this reading), so high directionality. The federal courts sit at an analytical seat — they are the agenda-setter but face high resistance from opposing doctrine and institutional pressure from Congress and the political branches.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments (beneficiaries) have institutional power, generational time horizon, and analytical exit — they are the reading's primary beneficiary seat and calculate directionality near 0.0 (subsidized). Civil rights claimants (victims) have powerless position, trapped exit (cannot leave the state or opt out of state law), biographical time horizon, and local scope — they face extraction from the withdrawal of federal authority and have high directionality near 1.0 (full target). Local businesses (beneficiaries) have organized power, constrained exit (must operate within some regulatory jurisdiction), and regional scope — they benefit from freedom from federal mandates but depend on state law, so moderate directionality around 0.3-0.4 (partial beneficiary). Interstate commerce operators (beneficiaries) have powerful position and mobile exit (can route business through favorable jurisdictions) and global scope, so low directionality near 0.1 (strong beneficiary). The federal courts have analytical power and exit — they are positioned as neutral interpreters, but their doctrinal choices produce asymmetric effects across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing balkanization by state tariffs while preserving state autonomy) was live in 1789-1850 and is substantially dead by 2026. The Dormant Commerce Clause and Supremacy Clause have established federal supremacy in genuinely interstate commerce. However, the narrow originalist reading has been revived (not created) in recent decades as a principled limit on federal power post-New Deal, so it cannot be classified as a piton (which requires theatrical maintenance of a function no one defends). The reading persists because originalist scholars and conservative judges actively defend it as the correct constitutional interpretation, even though it has not been fully implemented (federal environmental, labor, and civil rights laws remain largely intact). The reading is not mandatrophic in the sense of a dead founding problem: originalists argue the problem is live (federal power has become too extensive), while opponents argue the problem was genuinely solved (federal power is legitimate and necessary). This is constitutional disagreement, not mandatrophy. The mismatch check: founding_problem_status = contested, disappearance_verdict = contested — the mismatch is zero, so no zombie-flag applies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_vs_framers_intent,
    'Does ''original meaning'' refer to the original public meaning of the text at ratification, or the Framers'' subjective intent? If the Framers intended broader federal power than the public meaning of the text allowed, which governs?',
    'Historical evidence from ratification debates, Federalist Papers, state ratification conventions, and contemporary usage of ''commerce'' in 1789. Scholarly reconstruction of both public meaning and Framers'' intent separately, then comparison.',
    'If original public meaning governs, the narrow originalist reading has strong support from 18th-century trade law and commercial usage. If Framers'' intent governs, evidence from the Constitutional Convention suggests broader federal power (delegates worried about tariff wars and trade fragmentation, expected federal power to prevent state economic wars). The distinction determines whether the narrow reading is the correct originalism or a misreading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_vs_framers_intent, empirical, 'Whether originalism resolves to text-as-public-understood or Framers'' private intent.').

omega_variable(
    commerce_vs_economic_activity,
    'Does ''commerce'' in 18th-century usage refer narrowly to buying and selling (exchange of goods), or more broadly to economic activity? If a farmer growing wheat for sale engages in commerce, does a farmer growing wheat for family consumption + occasional sale?',
    'Lexicographical analysis of ''commerce'' in legal and non-legal texts from 1750-1800. Examination of how colonial and state law used the term. Analysis of whether the Framers distinguished commerce from production/manufacture.',
    'Narrow reading (commerce = exchange only, excluding production) supports the originalist position. Broader reading (commerce = all economic activity) supports broad-effects doctrine. This is empirically resolvable through historical usage studies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commerce_vs_economic_activity, empirical, 'The historical scope of ''commerce'' in 18th-century language.').

omega_variable(
    regulate_as_facilitate_vs_control,
    'In 18th-century legal and economic usage, does ''regulate'' mean ''make regular'' (remove barriers, facilitate trade) or ''impose rules and control''? The narrow reading relies on ''make regular'' as the primary meaning.',
    'Lexicographical analysis of ''regulate'' in contemporary legal texts (British, colonial, state law 1750-1800). Examine how ''regulation'' was used in mercantilist trade discourse versus police power discourse.',
    'If ''make regular'' is the dominant historical meaning, the narrow originalist reading is strengthened. If ''impose rules and control'' is equally or more common, the narrow reading''s linguistic foundation weakens and broad-effects doctrine gains support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulate_as_facilitate_vs_control, empirical, 'Historical meaning of ''regulate'' in legal usage at the Founding.').

omega_variable(
    federalism_as_structural_vs_as_value,
    'Is federalism a structural principle (states and federal government have separate, defined domains) or a value principle (decentralization is good because it preserves liberty and experimentation)? This distinction determines whether the narrow originalist reading is a neutral structural limit or a policy choice.',
    'Historical analysis of Framers'' federalism debates (Federalist/Anti-Federalist dispute, Convention debates). Examination of whether the structure was meant to reflect a judgment about good governance or merely to solve the problem of coordination without assuming Framers held a theory of federalism.',
    'If federalism is primarily structural (answering ''who decides?''), the reading is a neutral parsing of constitutional text. If federalism is primarily a value commitment (answering ''why should decentralization be preserved?''), the reading embeds a policy choice that could be revised if the value calculus changes. This affects whether the reading is a constraint (fixed by text) or a preference (contingent on values).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalism_as_structural_vs_as_value, conceptual, 'Whether federalism is structural or axiological.').

omega_variable(
    reading_vs_kernel_ambiguity,
    'Is the narrow originalist reading a correct reading of the Constitution''s kernel, or is it one reading among valid alternatives, each grounded in different hermeneutic premises? This omega documents the core contest.',
    'This is fundamentally not resolvable empirically or conceptually — it is a preference question about which hermeneutic methodology (originalism, living constitutionalism, traditionalism, purposivism) has legitimacy. Different reading communities accept different methodologies.',
    'If the narrow reading is THE correct reading, it functions as a discovered constraint (Mountain-like). If it is one reading among valid alternatives, it functions as an interpreted constraint (Rope/Tangled Rope, depends on its effects). This omega documents the irreducible methodological disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_kernel_ambiguity, preference, 'Is narrow originalism THE reading or A reading of the Commerce Clause?').

omega_variable(
    suppression_mechanism_structural_internalized,
    'Is the suppression of broad-effects doctrine due to structural constraints (the text genuinely limits federal power) or internalized (legal communities have adopted originalism as legitimate methodology and now screen out broad constructions as ''not valid law'' regardless of text)? This affects whether resistance to the reading reflects a real alternative or an internalized acceptance.',
    'Comparative analysis: do non-originalist judges and scholars still argue for broad doctrine openly, or do they accept the narrow reading as binding and argue only about its application? Do legislative advocates for broad federal power challenge the reading itself, or argue within its constraints? Post-Dobbs and post-NFIB v. Sebelius, has opposition to the reading increased?',
    'If suppression is structural, the reading is genuinely discovered and difficult to displace. If suppression is internalized, it could collapse if originalism lost legitimacy as a methodology. The distinction affects whether the constraint is stable or contingent on epistemic fashion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_internalized, empirical, 'Whether suppression of broad doctrine reflects text constraints or internalized acceptance of methodology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1789, commerce_clause_scope__narrow_originalist, theater_ratio, 1789, 0.08).
narrative_ontology:measurement(comm_tr_t1895, commerce_clause_scope__narrow_originalist, theater_ratio, 1895, 0.1).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__narrow_originalist, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__narrow_originalist, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__narrow_originalist, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(comm_tr_t2026, commerce_clause_scope__narrow_originalist, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(comm_be_t1789, commerce_clause_scope__narrow_originalist, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(comm_be_t1895, commerce_clause_scope__narrow_originalist, base_extractiveness, 1895, 0.35).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__narrow_originalist, base_extractiveness, 1937, 0.68).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__narrow_originalist, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__narrow_originalist, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(comm_be_t2026, commerce_clause_scope__narrow_originalist, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1789, commerce_clause_scope__narrow_originalist, suppression_requirement, 1789, 0.22).
narrative_ontology:measurement(comm_su_t1895, commerce_clause_scope__narrow_originalist, suppression_requirement, 1895, 0.18).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__narrow_originalist, suppression_requirement, 1937, 0.25).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__narrow_originalist, suppression_requirement, 1995, 0.31).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_scope__narrow_originalist, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(comm_su_t2026, commerce_clause_scope__narrow_originalist, suppression_requirement, 2026, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__narrow_originalist, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, dormant_commerce_clause).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, federal_civil_rights_enforcement_authority).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, federal_environmental_regulation_authority).

% DUAL FORMULATION NOTE:
% The commerce_clause_scope kernel decomposes into three constraint stories, each representing a distinct reading of 'commerce' and 'regulate.' Narrow originalist (this file) interprets commerce as trade crossing state lines and regulate as facilitation; broad effects interprets commerce to include any economic activity with substantial aggregate effects on interstate commerce and regulate as comprehensive control; intermediate channels interprets commerce to include channels/instrumentalities and activities substantially affecting interstate commerce with limiting principles. Each reading has different ε values, beneficiary/victim structures, and classified types. They coexist as competing live positions in constitutional law. This story (narrow originalist) influences the others by setting a baseline originalist interpretation; it forecloses neither the broad nor intermediate readings (both remain logically possible under different hermeneutic premises) but creates structural pressure by claiming to be the only textualist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
