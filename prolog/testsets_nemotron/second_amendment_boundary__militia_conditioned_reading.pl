% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment Boundary — Militia-Conditioned Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the militia-conditioned reading of the
 *   Second Amendment: the prefatory clause 'A well regulated Militia, being
 *   necessary to the security of a free State' defines and limits the scope
 *   of 'the right of the people to keep and bear Arms.' Under this reading,
 *   the right is collective, tied to organized militia service, and subject
 *   to comprehensive regulation by democratically accountable legislatures.
 *   The reading has been doctrinally dominant in lower courts post-Heller for
 *   upholding regulations, though the Supreme Court's Bruen (2022) decision
 *   shifted methodology toward historical analogy, creating tension. The
 *   constraint is the standing arrangement of judicial doctrine, legislative
 *   practice, and enforcement machinery that treats firearms as regulable
 *   instruments within a collective-defense framework.
 *
 * KEY AGENTS:
 *   - state_legislatures: Primary agenda_setter (institutional/arbitrage) — sets regulatory agenda under the reading's authorization
 *   - gun_owners_restricted_jurisdictions: Primary payer (moderate/constrained) — bears compliance costs and access restrictions
 *   - self_defense_claimants_high_regulation: Primary victim (powerless/trapped) — denied practical self-defense access
 *   - firearms_industry: Powerful payer (powerful/constrained) — subject to design and market restrictions
 *   - constitutional_originalists: Excluded (institutional/analytical) — individual-right reading displaced
 *   - insurrectionist_theorists: Excluded (moderate/identity_locked) — insurrectionist reading displaced
 *   - legal_scholars_second_amendment: Observer (analytical/analytical) — analyzes doctrinal coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.42).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.55).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment Boundary — Militia-Conditioned Reading").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '5d2257e6-de00-4526-903b-268b22fa4d3c').
narrative_ontology:cs_kernel_codification('5d2257e6-de00-4526-903b-268b22fa4d3c', fixed_text).
narrative_ontology:cs_authority_grounding('5d2257e6-de00-4526-903b-268b22fa4d3c', lineage).
narrative_ontology:cs_interpretation_layer_present('5d2257e6-de00-4526-903b-268b22fa4d3c').
narrative_ontology:cs_reading_relation('5d2257e6-de00-4526-903b-268b22fa4d3c', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d2257e6-de00-4526-903b-268b22fa4d3c', second_amendment_boundary__insurrectionist_reading, forecloses).
narrative_ontology:cs_axiom('5d2257e6-de00-4526-903b-268b22fa4d3c', foundational, prefatory_clause_defines_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_defines_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('5d2257e6-de00-4526-903b-268b22fa4d3c', prefatory_clause_defines_operative_scope, conventional).
narrative_ontology:cs_axiom('5d2257e6-de00-4526-903b-268b22fa4d3c', foundational, collective_defense_only_legitimate_purpose).
narrative_ontology:cs_axiom_status(collective_defense_only_legitimate_purpose, holdable).
narrative_ontology:cs_axiom_grounding('5d2257e6-de00-4526-903b-268b22fa4d3c', collective_defense_only_legitimate_purpose, conventional).
narrative_ontology:cs_axiom('5d2257e6-de00-4526-903b-268b22fa4d3c', secondary, democratic_regulation_presumptively_valid).
narrative_ontology:cs_axiom_status(democratic_regulation_presumptively_valid, holdable).
narrative_ontology:cs_axiom_grounding('5d2257e6-de00-4526-903b-268b22fa4d3c', democratic_regulation_presumptively_valid, instrumental).
narrative_ontology:cs_reference_frame('5d2257e6-de00-4526-903b-268b22fa4d3c', founding_militia_necessity).
narrative_ontology:cs_drift_state('5d2257e6-de00-4526-903b-268b22fa4d3c', post_bruen_historical_analogy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5d2257e6-de00-4526-903b-268b22fa4d3c', '2026-08-04T14:30:00Z').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, municipal_governments).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, gun_violence_prevention_advocates).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_owners_restricted_jurisdictions).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_collectors).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_high_regulation).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, competitive_shooters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, competitive_shooters).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_industry).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, collective_rights_interpretation).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, democratic_regulation_of_dangerous_instruments).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, textualist_prefatory_clause_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and enforce firearms regulations (bans, licensing, storage requirements, carry restrictions) under the militia-conditioned reading's authorization. The reading provides legal cover for comprehensive regulation; legislatures bear political costs but gain regulatory latitude.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_legislatures, agenda_setter,
    institutional, generational, arbitrage, regional).

% Implement local ordinances (safe storage, discharge bans, sensitive-place restrictions) enabled by the reading. Benefit from reduced gun violence externalities; constrained by state preemption laws and political pressure.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, municipal_governments, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, municipal_governments, beneficiary).

% Gain authority to treat firearms as regulable consumer products and public health hazards. The reading legitimizes epidemiological approaches to gun violence; agencies do not directly enforce but shape the regulatory agenda.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_health_agencies, beneficiary,
    organized, generational, mobile, national).

% Use the reading as doctrinal foundation for litigation and legislative campaigns. Collect status and funding from successful restrictions; exit is mobile (can shift focus to other issues).
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_violence_prevention_advocates, beneficiary,
    organized, biographical, mobile, national).

% Face bans, registration, waiting periods, and carry prohibitions in high-regulation jurisdictions. Compliance costs are high (legal fees, transfer restrictions, prohibited possessions); exit requires relocation or surrender. Identity-locked for some (hunting heritage, self-concept as armed citizen).
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_owners_restricted_jurisdictions, payer,
    moderate, biographical, constrained, regional).

% Subject to restrictions on categories, features, and transfer of collectible firearms. The reading treats collecting as outside militia purpose, hence regulable. Exit is constrained: surrender, modify, or relocate collections; market value destroyed by bans.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_collectors, payer,
    moderate, biographical, constrained, national).

% Denied practical access to firearms for self-defense by licensing regimes, storage mandates, and carry bans validated under this reading. Most structurally trapped: cannot easily relocate, lack political voice, bear immediate safety costs.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_high_regulation, payer,
    powerless, immediate, trapped, local).

% Face restrictions on competition firearms, magazine capacity, and transport. Benefit from ranges and clubs that persist under regulation; exit constrained by sport's equipment dependence and jurisdiction-specific rules.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, competitive_shooters, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, competitive_shooters, beneficiary).

% Subject to design mandates, marketing restrictions, dealer licensing, and market contractions from bans. Powerful lobbying capacity but constrained by regulatory capture of the reading's logic; exit requires product redesign or market abandonment.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_industry, payer,
    powerful, biographical, constrained, national).

% Advocate the individual-right reading as original public meaning; excluded from doctrinal dominance under this reading's judicial ascendancy. Their exclusion is the reading's structural condition.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_originalists, excluded,
    institutional, civilizational, analytical, national).

% Hold that the right preserves armed resistance capacity; excluded because the militia-conditioned reading treats insurrection as treason, not constitutional purpose. Identity-locked: worldview fuses the right with revolutionary legitimacy.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, insurrectionist_theorists, excluded,
    moderate, generational, identity_locked, national).

% Analyze the reading's textual, historical, and doctrinal coherence. Neither collect nor pay; their discipline's legitimacy partly depends on the contest remaining live.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, legal_scholars_second_amendment, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective defense capacity through state-organized militia structures; allocates regulatory authority over dangerous instruments to democratic bodies; provides a stable textual anchor for resolving firearms policy disputes through legislative process rather than judicial veto.
% TRANSFER_FUNCTION: Moves regulatory authority over firearms from individual possessors to state and local legislatures; moves compliance costs (licensing, registration, surrender, restricted access) onto gun owners, collectors, and industry; moves public safety benefits (reduced shootings, trafficking interruption) to general population.
% ABSENT_VOICES: Residents of high-violence neighborhoods who support both self-defense access and community safety measures but are represented by neither the gun-rights lobby nor the prevention-advocacy establishment; rural communities where firearms are cultural infrastructure and regulation feels like cultural erasure; future generations who inherit the constitutional settlement but cannot contest it.
% DISAPPEARANCE_RATIONALE: If the militia-conditioned reading vanished overnight, the individual-right reading would become doctrinally unopposed, triggering judicial invalidation of assault-weapon bans, magazine limits, universal background checks, carry restrictions, and sensitive-place laws across jurisdictions. The regulatory architecture built since Heller/McDonald would collapse; legislatures would lose presumptive authority; the firearms market would expand under constitutional protection. The world rearranges.
% FOUNDING_PROBLEM: The Founding generation's fear of a standing army and their reliance on citizen militias for defense created a constitutional provision tying the right to bear arms to militia service. The problem was ensuring states could maintain effective militias without federal disarmament.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Founding era (Rakove, Cornell, Waldman) attest the militia context was central and the standing-army fear has been obsolete since the 19th century. The Supreme Court's majority in Heller (Scalia, joined by Roberts, Kennedy, Thomas, Alito) acknowledged the prefatory clause announces a purpose but held it does not limit the operative clause — a contested reading, not a corroboration of the founding problem's persistence. No living constitutional actor asserts the original militia problem remains live; the reading's persistence rests on its utility for democratic regulation, not its historical fidelity.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).
:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects the reading's transfer of regulatory authority from individuals to states — moderate because the coordination function (collective defense regulation) is genuine and the extraction falls on a specific class (gun owners) rather than the whole population. Suppression (0.55) is higher because the reading's persistence requires active judicial and legislative enforcement against challenges from individual-right and insurrectionist readings; the Bruen methodology shift increased suppression requirements. Theater ratio (0.28) is moderate: the militia rationale is largely vestigial (no functioning state militias exist), but the reading performs the coordination function of democratic firearms regulation. Accessibility collapse (0.48) is moderate: alternatives (individual-right, insurrectionist readings) remain live in public discourse and judicial dissent but are structurally excluded from doctrinal operation in restrictive jurisdictions. Resistance (0.62) is high: the reading faces sustained litigation, legislative pushback, and scholarly critique from multiple directions.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (state_legislatures, municipal_governments) experience this as a coordination constraint enabling democratic governance of dangerous instruments — extraction is negative (they gain authority). The payer seats (gun_owners_restricted_jurisdictions, firearms_collectors, self_defense_claimants_high_regulation) experience it as extraction — they lose access, face compliance costs, and have constrained exit. The excluded seats (constitutional_originalists, insurrectionist_theorists) experience it as foreclosure — their preferred readings are structurally displaced. The observer seat (legal_scholars) sees the full structure. The engine computes per-seat effective extraction from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state_legislatures, municipal_governments, public_health_agencies, gun_violence_prevention_advocates) collect regulatory authority, public safety externalities, or advocacy wins — directionality d near 0.0 (beneficiary end). Victims (gun_owners_restricted_jurisdictions, firearms_collectors, self_defense_claimants_high_regulation, competitive_shooters, firearms_industry) bear compliance costs, access restrictions, market contractions — directionality d near 1.0 (target end), modulated by exit options: self_defense_claimants_high_regulation are trapped (d ≈ 1.0), gun_owners_restricted_jurisdictions are constrained (d ≈ 0.8), firearms_industry is constrained but powerful (d ≈ 0.7). Excluded agents are not in the directionality computation for χ; their structural displacement is a separate fact. The engine derives d from these declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state militia viability against federal disarmament) is dead — standing armies and National Guard have replaced the militia system. The arrangement persists because it solves a new coordination problem (democratic regulation of firearms) that the founding generation did not anticipate. This is classic mandatrophy: the constraint's current function (enabling comprehensive firearms regulation) is not its founding function (protecting state militias). The reading's beneficiaries (state_legislatures, prevention_advocates) are not the founding beneficiaries (state militias). The classification as tangled_rope (not snare) captures this: genuine coordination function (democratic regulation of dangerous instruments) coexists with asymmetric extraction (gun owners bear costs). If the coordination function were acknowledged as the true justification, the constraint might be re-authored as scaffold with a sunset clause tied to empirical validation of regulation efficacy — but no such sunset exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_conditioned_naturalness,
    'Is the militia-conditioned reading a genuine textual/historical necessity, or a constructed interpretation that benefits state regulatory authority?',
    'Comparative historical analysis of Founding-era usage of ''bear arms'' and ''militia'' across state constitutions, congressional debates, and militia acts; whether the textual structure admits only the collective reading or permits the individual reading as equally plausible.',
    'If the reading is historically necessitated, it approaches mountain status (low extractiveness, high accessibility_collapse). If constructed, it is a tangled_rope or snare whose beneficiaries (state legislatures) gain regulatory authority from a tendentious reading. This is the false_summit_mountain question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_conditioned_naturalness, conceptual, 'Whether the militia-conditioned reading''s textual naturalness is genuine or constructed.').

omega_variable(
    coordination_extraction_separability,
    'Can the democratic regulation coordination function be separated from the asymmetric extraction on gun owners, or are they structurally fused?',
    'Counterfactual: if a regulatory regime achieved public safety goals without burdening lawful possessors (e.g., focused on trafficking, straw purchases, prohibited possessors), would the reading''s proponents accept it? Or is the burden on lawful possessors the point?',
    'If separable, the extraction is contingent and the constraint could evolve toward rope. If fused, the extraction is structural and the tangled_rope classification is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are separable in practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (judicial enforcement, legislative enactment) or internalized (gun owners'' compliance born of legal fatalism, cultural marginalization)?',
    'Post-Bruen compliance trajectory: if suppression persists or increases despite doctrinal shifts favoring individual-right readings, internalized suppression is significant. Survey data on gun owners'' perceived legitimacy of regulations.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than institutional measures suggest — the target population carries the suppression with them, reducing resistance and enabling deeper extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in gun owner compliance.').

omega_variable(
    reading_relations_foreclosure_strength,
    'Does the militia-conditioned reading genuinely foreclose the insurrectionist reading in a single framework, or do they coexist as mutually unintelligible but both live positions?',
    'Analyze whether any constitutional framework (state constitution, proposed amendment, scholarly synthesis) has successfully integrated both premises. Check if insurrectionist theorists operate within the militia-conditioned framework or reject it entirely.',
    'If genuine foreclosure, the reading_relations foreclosure edge is structurally correct. If coexistence, the relation should be coexists_with, and the kernel''s constraint family has three mutually unintelligible live readings rather than a foreclosure pair.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_relations_foreclosure_strength, conceptual, 'Whether militia-conditioned and insurrectionist readings foreclose or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_tr_t1791, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1791, 0.02).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_tr_t1868, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_tr_t1934, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1934, 0.12).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_tr_t1968, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1968, 0.18).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_tr_t1994, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1994, 0.24).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_tr_t2008, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2008, 0.26).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_tr_t2010, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_tr_t2022, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2022, 0.28).

% Extraction over time
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_be_t1791, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1791, 0.05).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_be_t1868, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1868, 0.08).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_be_t1934, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1934, 0.15).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_be_t1968, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1968, 0.22).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_be_t1994, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1994, 0.31).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_be_t2008, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2008, 0.35).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_be_t2010, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_be_t2022, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2022, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_su_t1791, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_su_t1868, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1868, 0.15).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_su_t1934, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1934, 0.28).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_su_t1968, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1968, 0.35).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_su_t1994, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1994, 0.44).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_su_t2008, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_su_t2010, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2010, 0.51).
narrative_ontology:measurement(second_amendment_boundary__militia_conditioned_reading_su_t2022, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2022, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__militia_conditioned_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, national_firearms_act_regime).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, gun_control_act_1968_regime).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, brady_handgun_violence_prevention_act).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, assault_weapons_ban_1994).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, bruen_historical_analogy_doctrine).

% DUAL FORMULATION NOTE:
% The second_amendment_boundary kernel decomposes into three constraint stories: militia_conditioned_reading (this story, tangled_rope, ε=0.42), individual_right_reading (rope/tangled_rope boundary, ε≈0.25), insurrectionist_reading (snare-adjacent, ε≈0.65). The ε values differ because each reading instantiates a different constraint with different beneficiary/victim structures and enforcement requirements. This story's ε reflects the extraction of the regulatory regime it authorizes; the individual_right_reading's ε reflects the extraction of the judicial veto it empowers; the insurrectionist_reading's ε reflects the extraction of the revolutionary capacity it claims. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__militia_conditioned_reading, institutional, 0.1).
constraint_indexing:directionality_override(second_amendment_boundary__militia_conditioned_reading, powerless, 0.95).
constraint_indexing:directionality_override(second_amendment_boundary__militia_conditioned_reading, moderate, 0.75).
constraint_indexing:directionality_override(second_amendment_boundary__militia_conditioned_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
