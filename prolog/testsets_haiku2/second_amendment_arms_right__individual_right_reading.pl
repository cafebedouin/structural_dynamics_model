% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Individual Right to Keep and Bear Arms (Second Amendment)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The individual-right reading of the Second Amendment holds that the right
 *   to keep and bear arms is a pre-political natural liberty, retained by
 *   individuals against government infringement, and protected by
 *   constitutional text from federal (and via incorporation, state)
 *   prohibition or severe restriction. This reading triumphed in judicial
 *   doctrine with District of Columbia v. Heller (2008) and McDonald v. City
 *   of Chicago (2010), overturning decades of collective-right jurisprudence.
 *   It instantiates a constitutional boundary: individuals benefit from the
 *   protected right; federal and state regulatory authorities bear the cost
 *   of surrendered discretion; public health constituencies face narrowed
 *   policy options. The constraint is CLAIMED as tangled_rope (coordination
 *   of constitutional boundary + asymmetric extraction of regulatory power)
 *   while measurements document active enforcement suppression and rising
 *   extractiveness as the reading entrenches. This is a kernel reading — one
 *   interpretation of the contested Second Amendment text; sibling readings
 *   (collective-right, civic-republican) instantiate different constraints
 *   with different beneficiary structures and victim sets.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Primary beneficiary (constitutional protection of ownership right; constrained exit through relocation only)
 *   - federal_regulatory_authority: Primary payer (surrendered prohibition authority; constrained power to regulate through constitutionalization of the limit)
 *   - state_regulatory_authority: Secondary payer (lost police-power discretion over firearms; must administer enforcement of limits on its own authority)
 *   - firearms_manufacturers_dealers: Secondary beneficiary (protected market; mobile exit via multi-state operations)
 *   - public_health_advocates: Tertiary payer (policy toolkit narrowed to non-prohibition regulation)
 *   - supreme_court_doctrine: Agenda-setter (establishes and enforces the reading's boundaries through constitutional interpretation)
 *   - collective_right_reading_constituency: Excluded (structurally barred from legal implementation by Heller; remain political voices only)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.71).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Individual Right to Keep and Bear Arms (Second Amendment)").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, '9807e18f-8a7b-44a3-b0f6-4cbdb6f38697').
narrative_ontology:cs_kernel_codification('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', fixed_text).
narrative_ontology:cs_authority_grounding('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', lineage).
narrative_ontology:cs_interpretation_layer_present('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697').
narrative_ontology:cs_reading_relation('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', second_amendment_arms_right__civic_republican_reading, influences).
narrative_ontology:cs_axiom('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', foundational, right_precedes_government).
narrative_ontology:cs_axiom_status(right_precedes_government, holdable).
narrative_ontology:cs_axiom_grounding('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', right_precedes_government, deontological).
narrative_ontology:cs_axiom('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', foundational, individual_not_militia_centered).
narrative_ontology:cs_axiom_status(individual_not_militia_centered, holdable).
narrative_ontology:cs_axiom_grounding('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', individual_not_militia_centered, deontological).
narrative_ontology:cs_reference_frame('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', natural_law_pre_political_right).
narrative_ontology:cs_drift_state('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', post_heller_doctrine_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9807e18f-8a7b-44a3-b0f6-4cbdb6f38697', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_regulatory_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_manufacturers_dealers).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, constitutional_scholars_individualist).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, public_health_advocates).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, natural_law_pre_government_rights).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, individual_liberty_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim a pre-political natural right to possess firearms for self-defense, sport, and resistance to tyranny. This reading protects them from federal prohibition and, via incorporation, from absolute state bans. They benefit from constitutional recognition that shields their ownership from confiscatory regulation. Their exit is constrained: migration to permissive jurisdictions is costly and incomplete given federal reach.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, constrained, national).

% Under this reading, the federal government is constitutionally barred from prohibiting or substantially restricting individual gun ownership. It bears the cost of surrendering regulatory supremacy over firearms, accepting a constitutional veto on policy choices that other democracies exercise (e.g., near-total bans, licensing for all owners). Its power is constrained by a pre-political limit, not merely by a legislative supermajority or political negotiation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority, payer,
    institutional, generational, constrained, national).

% Historically claimed police power over firearms (permitting, carry regulation, licensing). The individual-right reading and its incorporation via the Fourteenth Amendment constrain state discretion: states cannot ban handguns, cannot impose total prohibitions, must respect core self-defense uses. States can still regulate (permitting, background checks, licensing for carry), but the baseline is set by constitutional protection of individual ownership, not state permission to own. They must administer enforcement of limits on their own authority.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_regulatory_authority, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__individual_right_reading, state_regulatory_authority, agenda_setter).

% Benefit from a large protected market of individual gun owners. Constitutional protection of ownership translates into market demand; a reading that treats the right as pre-political and individual protects their customers' purchasing power. They have the most exit mobility (relocation to permissive states, international markets) and profit from the protected market.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_manufacturers_dealers, beneficiary,
    powerful, biographical, mobile, national).

% Bear the regulatory constraint: they cannot advocate for or implement policies (universal bans, licensing requirements that approximate prohibition, near-total confiscation) that other wealthy democracies use. Gun-violence reduction through restriction is constitutionally bounded by this reading. Their policy toolkit is narrowed to background checks, carry licensing, and narrower regulations that do not implicate core self-defense.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, public_health_advocates, payer,
    organized, biographical, constrained, national).

% Advocates for the reading that the Second Amendment protects state militia authority only, not individual ownership outside militia context. They are structurally excluded from the legal determination by the Supreme Court's 2008 Heller decision, which affirmed the individual-right reading. Their alternative reading is no longer a live constitutional option in federal law, though it remains a live political position and influences state constitutional reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, collective_right_reading_constituency, excluded,
    organized, biographical, trapped, national).

% The judicial institution that interprets the Second Amendment and enforces this reading's boundaries. It has set the baseline (individual right to keep and bear arms for lawful purposes including self-defense) and continues to adjudicate what regulations survive scrutiny.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, supreme_court_doctrine, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_arms_right__individual_right_reading, supreme_court_doctrine).

% Academic and intellectual figures who advocate for the natural-law, individual-right reading of the Second Amendment. They benefit from judicial adoption of their interpretive frame; it vindicates their scholarship and influence policy through the doctrine they helped establish.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, constitutional_scholars_individualist, beneficiary,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading does NOT coordinate a collective action problem in the conventional sense. Instead, it vindicates a constitutional boundary: it protects individuals' pre-political right to arms against government monopoly. The coordination story it supports is the meta-constitutional one — that individual rights pre-exist government and constrain it, rather than government granting rights to citizens. The problem it solves is preventing tyranny through an armed citizenry and preserving self-defense capacity outside state permission.
% TRANSFER_FUNCTION: Moves regulatory authority FROM federal and state governments TO individual gun owners. The transfer is power/discretion: governments surrender the ability to prohibit or near-wholly restrict individual ownership in the name of constitutional limitation. Individual gun owners gain the power to own firearms as a matter of constitutional right rather than state license. Manufacturers/dealers gain protected market access. Public health authorities lose the policy toolkit of confiscatory regulation.
% ABSENT_VOICES: The collective-right reading constituency (those who would read the Second Amendment as protecting state militia authority only) is structurally excluded by Heller and subsequent doctrine. They would argue that the individual-right reading misconceives the historical text and misplaces liberty in gun ownership rather than in state militia service. Those harmed by gun violence and advocates for public health restrictions are marginalized — their participation is permitted but their policy preferences are constitutionally constrained.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment disappeared — if the individual-right interpretation were overturned and the collective-right reading (state militia only) were installed — the political economy of firearms would reorganize: federal and state governments would have constitutional discretion to ban handguns, require universal licensing and confiscation, and regulate ownership to near-prohibition levels, as other democracies do. Millions of gun owners would face either confiscation, criminalization, or relocation. Manufacturers would lose the protected market. The constraint's disappearance would not merely alter regulation; it would enable a regime change in firearms policy.
% FOUNDING_PROBLEM: The founding problem is dual-layered: (1) the historical problem of republican self-governance — ensuring an armed citizenry as a check on tyranny (Founders' framing); (2) the philosophical problem of pre-political natural rights — protecting liberty that pre-exists government against government encroachment. This reading claims that the Second Amendment codifies protection of an individual liberty that existed prior to and independent of governmental creation.
% FOUNDING_PROBLEM_CORROBORATION: The individual-right reading is corroborated by the Supreme Court (Heller, 2008; McDonald, 2010; Bruen, 2022) and by originalist constitutional scholars (Antonin Scalia, David Kopel, Eugene Volokh). The collective-right reading is corroborated by historical scholars (Saul Cornell, Michael Waldman) and public health researchers who contest the natural-law premise. The Founders' intent is itself contested: some scholars argue the Founders' primary concern was militia service; others argue individual self-defense and resistance to tyranny were central. No consensus exists outside the benefiting (individual-right) constituency.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval, reflecting increasing entrenchment of the individual-right reading and expansive Supreme Court doctrine (Heller, McDonald, Bruen). Early extractiveness is moderate because the reading's implications are contested and regulatory authority retains significant discretion through permitting and licensing regimes. Mid-interval (t=20) marks McDonald (2010), which incorporates the right against states; extractiveness jumps as state police power is constitutionally constrained. Late-interval (t=30+) shows Bruen (2022), which strikes down historical acquiescence defenses and expands protection; extractiveness plateaus as the reading's doctrine reaches full development and resistance crystallizes. Suppression remains high (0.55–0.71) because maintaining the individual-right reading requires continuous judicial defense against legislative challenge and collective-right interpretation; the constraint is not natural law but enforced constitutional doctrine. Theater rises from 0.25 to 0.42 as the doctrine matures: early enforcement is substantive (actual constitutional limits on regulation); later enforcement becomes increasingly performative (defending against collective-right challenges, managing regulatory workarounds, accommodating historical regulations). The temporal trajectory traces NOT the birth of an individual right (defenders claim it was always there) but the historical RECOVERY and ENTRENCHMENT of the reading in American law.
 *
 * PERSPECTIVAL GAP:
 *   From the individual gun owner's perspective, this reading is a liberation: recognition of a pre-political right that the government had wrongfully denied. From the federal/state regulatory authority's perspective, this reading is a constraint: the loss of policy discretion that other democracies retain. From the public health advocate's perspective, it is a capture: the constitution is weaponized to shield gun ownership from regulation. The engine computes these divergences from the power atoms, exit options, and beneficiary/victim declarations. The individual gun owner sits at (powerless-to-moderate power, constrained exit, near-beneficiary d); the federal authority sits at (institutional power, constrained exit, near-target d because the constraint vetos its policy choices). The public health advocate sits at (organized power, constrained exit, near-target d because regulation is narrowed). The perspectival gap is structural, not merely evaluative.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners: d ≈ 0.25–0.35. They are beneficiaries (gain protected ownership right), but they are also constrained (cannot exit the U.S. without relocation). Their beneficiary status is primary; directionality pushes them toward subsidy and away from target. Federal regulatory authority: d ≈ 0.75–0.85. The constraint directly targets this seat by veto-ing its policy options (prohibition, near-total bans). Institutional power does not save it because the constraint is a constitutional limit, not a political negotiation. State regulatory authority: d ≈ 0.70–0.80. Similar to federal, but slightly lower because states retain some permitting and licensing discretion; the constraint is tighter post-incorporation but not absolute. Public health advocates: d ≈ 0.60–0.70. They are not formal stakeholders in the constitution but their policy preferences are targeted by the constraint; it narrows their toolkit to regulation-short-of-prohibition. Manufacturers: d ≈ 0.05–0.15 (near-beneficiary). They benefit from protected market access; their mobile exit option (multi-state operations, international markets) lowers their d further.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy assessment: Does the founding problem (republican armed citizenship, pre-political natural rights) still exist? The constraint claims to protect against tyranny and government monopoly on force. The founding problem STATUS is contested: individual-right advocates argue the problem is live (government pressure on ownership persists; constitutional protection remains necessary); collective-right advocates and public health scholars argue the founding problem has shifted (modern tyranny is less military than administrative; gun violence is now the tyranny problem; the constraint outlives its justification). The classification as tangled_rope (not rope) reflects this: the reading coordinates the meta-constitutional principle (individual rights pre-exist government) AND extracts regulatory power asymmetrically (beneficiaries gain protection; victims lose discretion). The theater trajectory (rising from 0.25 to 0.42) suggests increasing performativity: the Supreme Court's defense of the doctrine against legislative challenge becomes more theatrical, defending an established position rather than protecting a threatened one. No formal mandatrophy exists; the founding problem remains live within one constitutional tradition and dead within another — the mismatch is read off the sibling readings, not off this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_positive_law,
    'Is the right to keep and bear arms a pre-political natural law (existing prior to government), or a positive law created by constitutional text and judicial interpretation?',
    'Genealogical and historical analysis of the right''s grounding in philosophy (Locke, natural law traditions) vs. its instantiation in constitutional text and common law precedent. Challenge: natural-law claims are inherently non-falsifiable; the resolution is philosophical/interpretive, not empirical.',
    'If the right is natural law, the individual-right reading is correct in principle and the constraint represents recognition of a pre-political boundary. If it is positive law, the reading is a judicial creation that can be judicially overturned; the constraint''s persistence depends on institutional entrenchment, not on natural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_positive_law, conceptual, 'Whether the Second Amendment protects a pre-existing natural right or constitutive positive law.').

omega_variable(
    tyranny_prevention_vs_gun_violence_problem,
    'In contemporary democracy, is the founding problem (prevention of government tyranny through armed citizenry) still the primary concern, or has the problem shifted to gun violence prevention?',
    'Comparative analysis of firearm mortality across regimes (democracies with near-total bans vs. armed-citizenry regimes) and empirical assessment of which regime structure correlates with tyranny risk and gun violence. The problem is that both correlates exist, and the causal mechanisms are contested.',
    'If tyranny prevention is live, the constraint''s protection of individual ownership is justified by the founding problem; if gun violence is the primary problem, the constraint represents mandatrophy — a limit protecting against a historical threat that has metastasized into a different threat (violence, not tyranny).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tyranny_prevention_vs_gun_violence_problem, empirical, 'Whether the founding problem of tyranny prevention remains live or has been displaced by gun violence as the primary policy concern.').

omega_variable(
    regulatory_discretion_loss_quantification,
    'How much policy discretion has federal and state regulatory authority actually lost due to this reading? Can substantive regulation (permitting, licensing, background checks, carry restrictions) function as an alternative to prohibition?',
    'Empirical analysis of post-Heller regulations that have survived constitutional scrutiny and those that have been struck down. Comparison of permitting/licensing regimes across states to measure regulatory discretion retained despite the individual-right reading.',
    'If substantial discretion is retained through permitting and licensing, the extractiveness measure may be overstated — the constraint bans prohibition but permits robust regulation, and the regulatory authority is not as fully constrained as d=0.75+ suggests. If nearly all discretion is lost at the margin, the d value holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_discretion_loss_quantification, empirical, 'Whether regulatory authority actually loses discretion or merely loses the extreme option of total prohibition.').

omega_variable(
    collective_right_reading_suppression,
    'Is the collective-right reading genuinely a live legal option, or is it so thoroughly excluded by Heller and subsequent doctrine that it cannot be revived without constitutional amendment?',
    'Assess the doctrinal pathway for a Supreme Court reversal of Heller and the political feasibility of such a reversal. Determine whether the collective-right reading remains contestable within the law or has been judicially foreclosed.',
    'If the collective-right reading is genuinely excluded, the suppression measure (0.71) may be understated — the constraint is not merely enforced but enforces a one-way legal regime where only the individual-right reading is permitted. If it remains contestable, suppression is correctly measured as enforcement against political challenge, not judicial closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_right_reading_suppression, empirical, 'Whether the collective-right reading has been judicially foreclosed or remains a live constitutional option.').

omega_variable(
    reading_identity_kernel_foreclosure,
    'Does the individual-right reading logically foreclose the collective-right reading (two interpretations cannot coexist in one legal framework), or do they coexist as different readings of an ambiguous text?',
    'Textual and structural analysis: can the Second Amendment text coherently protect both (1) individual ownership for self-defense, AND (2) state militia authority as the primary right, simultaneously? Or does protecting one necessarily negate the other?',
    'If the readings foreclose each other (binary choice), the relation is ''forecloses'' and the constraint represents judicial victory for one side. If they coexist (ambiguous text read differently by different parties), the relation is ''coexists_with'' and the constraint represents entrenchment of one reading without logical closure of the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_kernel_foreclosure, conceptual, 'Whether the individual-right and collective-right readings are logically incompatible (foreclose) or merely different readings of the same text (coexist).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__individual_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__individual_right_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__individual_right_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(seco_tr_t20, observed).
narrative_ontology:measurement(seco_tr_t30, second_amendment_arms_right__individual_right_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(seco_tr_t30, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__individual_right_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(seco_tr_t40, observed).
narrative_ontology:measurement(seco_tr_t50, second_amendment_arms_right__individual_right_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(seco_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__individual_right_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__individual_right_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__individual_right_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(seco_be_t20, observed).
narrative_ontology:measurement(seco_be_t30, second_amendment_arms_right__individual_right_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(seco_be_t30, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__individual_right_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(seco_be_t40, observed).
narrative_ontology:measurement(seco_be_t50, second_amendment_arms_right__individual_right_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(seco_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__individual_right_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__individual_right_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__individual_right_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(seco_su_t20, observed).
narrative_ontology:measurement(seco_su_t30, second_amendment_arms_right__individual_right_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(seco_su_t30, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__individual_right_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(seco_su_t40, observed).
narrative_ontology:measurement(seco_su_t50, second_amendment_arms_right__individual_right_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(seco_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel admits three structurally distinct readings instantiating three constraint stories: (1) individual_right_reading (this story) — beneficiaries: individual gun owners; victims: federal/state regulatory authority; high ε on prohibition. (2) collective_right_reading — beneficiaries: state militia institutions; victims: individual gun owners; low/medium ε on individual ownership rights. (3) civic_republican_reading — beneficiaries: armed citizenry in civic-duty capacity; victims: neither primary, but regulatory authority is constrained by civic obligation. The three stories are linked: this reading (individual-right) has judicially displaced the collective-right reading and influences the civic-republican reading by establishing the baseline of individual ownership protection. The readings coexist politically (both have constituencies) but the individual-right reading dominates judicially. Each story authors its own ε, beneficiary/victim structure, and cs_structure reading_relations independently; the network link records the kernel kinship and structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__individual_right_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
