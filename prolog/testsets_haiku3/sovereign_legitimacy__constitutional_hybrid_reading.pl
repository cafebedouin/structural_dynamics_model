% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid Sovereign Legitimacy (Ceremonial + Delegated)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'sovereign legitimacy': the constitutional hybrid reading. The constraint
 *   describes a state where legitimate authority is dual-sourced —
 *   ceremonial/symbolic authority is inherited through dynastic succession,
 *   while political authority is delegated through electoral processes, with
 *   constitutional law explicitly mediating the boundary between these two
 *   spheres (exemplified by constitutional monarchies such as the UK, Spain,
 *   Netherlands, Belgium). This reading is DISTINCT from the monarchical
 *   reading (which locates all legitimacy in inherited sovereignty) and the
 *   republican reading (which locates all legitimacy in popular consent). The
 *   hybrid reading instantiates a different constraint with different
 *   beneficiary/victim structure, different ε, different persistence
 *   mechanism, and different vulnerability profile. Per Rule 1, this
 *   constraint is authored as a single, clean, ε-invariant story for ONLY
 *   this reading; the sibling readings are OTHER constraints (other files).
 *   The kernel contest is routed to omega variables and cs_structure per
 *   Rules 2–4.
 *
 * KEY AGENTS:
 *   - Hereditary monarch: retains ceremonial headship, symbolic authority, defined veto/assent powers, and dynastic prestige; identity-locked to the role; beneficiary of the arrangement's stability
 *   - Elected executive officials: exercise delegated political authority; benefit from constitutional grounding of their legitimacy; mobile exit (electoral cycles) but constrained by constitutional deference to monarch's ceremonial role
 *   - Citizenry: receive coordination benefit (stable dual-legitimacy state) but pay through maintaining the ceremonial apparatus and constraint on pure-form alternatives
 *   - Absolutist monarchists: constrained victims; believe legitimacy should flow entirely downward and resist delegation to elected bodies
 *   - Republican democrats: constrained victims; believe legitimacy should flow entirely upward and resist retention of hereditary authority
 *   - Constitutional interpreters (courts, legal scholars): the mechanism by which the boundary between ceremonial and political authority is maintained and adjudicated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.42).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.38).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid Sovereign Legitimacy (Ceremonial + Delegated)").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '453ab86a-95da-4160-86f1-fd5f988e0034').
narrative_ontology:cs_kernel_codification('453ab86a-95da-4160-86f1-fd5f988e0034', fixed_text).
narrative_ontology:cs_authority_grounding('453ab86a-95da-4160-86f1-fd5f988e0034', lineage).
narrative_ontology:cs_interpretation_layer_present('453ab86a-95da-4160-86f1-fd5f988e0034').
narrative_ontology:cs_reading_relation('453ab86a-95da-4160-86f1-fd5f988e0034', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('453ab86a-95da-4160-86f1-fd5f988e0034', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_axiom('453ab86a-95da-4160-86f1-fd5f988e0034', foundational, dual_legitimacy_sources_inseparable).
narrative_ontology:cs_axiom_status(dual_legitimacy_sources_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('453ab86a-95da-4160-86f1-fd5f988e0034', dual_legitimacy_sources_inseparable, conventional).
narrative_ontology:cs_axiom('453ab86a-95da-4160-86f1-fd5f988e0034', foundational, constitutional_law_mediates_boundaries).
narrative_ontology:cs_axiom_status(constitutional_law_mediates_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('453ab86a-95da-4160-86f1-fd5f988e0034', constitutional_law_mediates_boundaries, conventional).
narrative_ontology:cs_reference_frame('453ab86a-95da-4160-86f1-fd5f988e0034', constitutional_separation_of_sources).
narrative_ontology:cs_drift_state('453ab86a-95da-4160-86f1-fd5f988e0034', contemporary_republican_pressure, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('453ab86a-95da-4160-86f1-fd5f988e0034', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_executive_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_democrats).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, citizenry_as_constituents).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, citizenry_as_constituents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial authority, symbolic headship of state, constitutional veto power in defined contexts, income from crown lands or state stipend, and hereditary prestige. Delegates political (legislative/executive) authority to elected officials but maintains the power to interpret and defend the constitutional boundary between ceremonial and political spheres. Cannot exit the role without constitutional amendment; identity as sovereign is inseparable from lineage and dynastic continuity.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, agenda_setter).

% Exercise delegated political authority through elected office: set policy, command executive action, propose legislation. Benefit from the arrangement's stability and the constitutional grounding of their legitimacy in popular delegation. They govern in the monarch's name in ceremonial contexts but operate independently on substantive policy. Can exit via electoral defeat or term limits, but staying in office requires constitutional deference to the sovereign's defined ceremonial roles.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_executive_officials, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, elected_executive_officials, agenda_setter).

% Receive the arrangement's coordination function: a stable state with dual legitimacy sources that prevents either pure monarchy (where they would have no role) or pure democracy (where ceremonial continuity would dissolve). They also bear the cost of maintaining the ceremonial apparatus and tolerating the monarch's retained powers. They vote for elected officials but cannot directly alter the hereditary succession or the constitutional boundary.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, citizenry_as_constituents, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, citizenry_as_constituents, payer).

% Believe legitimate authority should flow entirely downward from the inherited sovereign and resent the delegation of political power to elected bodies. Under the hybrid arrangement, they are constrained: the monarch's power is limited, elections proceed despite their objections, and the constitutional text that encodes the hybrid explicitly forecloses pure monarchy. They can advocate for constitutional amendment but cannot change the standing arrangement unilaterally.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists, payer,
    moderate, civilizational, constrained, national).

% Believe legitimate authority should flow entirely upward from popular sovereignty and resent the retention of hereditary authority. Under the hybrid arrangement, they are constrained: the monarch retains ceremonial and limited veto powers, elections do not fully determine state headship, and the constitutional text that encodes the hybrid explicitly forecloses pure republicanism. They can campaign for constitutional amendment and have in some contexts (Ireland, Italy removed the monarchy; others retain it) but cannot change the standing arrangement unilaterally.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_democrats, payer,
    powerful, generational, constrained, national).

% Courts, legal scholars, and constitutional review bodies interpret where the boundary between ceremonial and political authority lies. They do not collect rents from the arrangement but they are the mechanism through which it persists: they define which powers are 'ceremonial' (reserved to the monarch) and which are 'political' (exercised by elected officials). Disputes about this boundary are adjudicated by them, making their interpretation power structurally central to the constraint.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Those who reject both the inherited monarch and the elected officials (revolutionary movements, alternative dynasties, secessionist groups) are structurally excluded from the hybrid framework. The arrangement enforces their exclusion by treating both monarchy and republicanism as jointly legitimate, leaving no standing for claims outside that binary.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, rival_legitimacy_claimants, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__constitutional_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of grounding state authority in two distinct but complementary sources: the symbolic/ceremonial authority of inherited continuity (which provides institutional stability and historical legitimacy) and the political authority of delegated popular consent (which provides responsiveness and democratic participation). The hybrid prevents either source from monopolizing legitimacy and prevents the state from degenerating into either pure authoritarianism or pure populism.
% TRANSFER_FUNCTION: Moves ceremonial/symbolic authority and associated privileges to the hereditary monarch (status, income, defined veto powers, headship of state) while moving political/executive authority to elected officials (policy-setting, legislative initiative, day-to-day governance). The constitution mediates the boundary by defining which powers each seat holds. Citizens pay through taxes (supporting both the monarch and elected officials) and through constraint on alternative legitimacy claims (neither pure monarchy nor pure republicanism is available).
% ABSENT_VOICES: Absolutist monarchists (who would restore pure hereditary authority) and republican democrats (who would eliminate the monarch entirely) are both present but constrained by the constitutional framework. Voices truly absent are revolutionary alternatives (those seeking to replace both sources of legitimacy with a new regime) and those seeking to dissolve the state entirely. Their exclusion is structural to the hybrid arrangement — it rules out both pure forms and thus rules out claims from those who advocate for pure forms as the legitimate endpoint.
% DISAPPEARANCE_RATIONALE: If the constitutional hybrid suddenly dissolved, states that maintain it would face immediate constitutional crises: the dual legitimacy grounding would collapse, and the successor regime would have to reconstitute authority either through monarchy restoration (reversing democratic gains) or republican amendment (overriding hereditary succession). The symbolic/institutional continuity currently provided by the monarch would need replacement. The arrangements that depend on the monarch's ceremonial headship (state visits, treaty signature authority, role in ceremonial pageantry) would require renegotiation. Citizens would face a binary choice between pure forms that the hybrid currently avoided.
% FOUNDING_PROBLEM: How can a state ground its authority in both inherited legitimacy (which provides stability and historical continuity) and delegated popular legitimacy (which provides responsiveness and participation), without one source monopolizing and excluding the other? How can tradition and innovation coexist in the same authority structure?
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars from outside the benefiting parties (comparative constitutional law experts, political philosophers, historians of constitutional amendment) attest that the founding problem remains live: states that attempted pure monarchy devolved into authoritarianism and instability; states that attempted pure republicanism faced legitimacy challenges around property, tradition, and ceremonial needs. The hybrid represents an attempt to hold both. However, the problem is contested — republicans argue the monarch is purely ceremonial and can be eliminated with no loss of coordination; monarchists argue the elected officials lack legitimacy without the sovereign's blessing.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end, stable across the interval) because the arrangement is genuinely a compromise: both monarchy and republicanism extract rents in their pure forms (absolute monarchy extracts by monopolizing authority; pure republicanism extracts by erasing the symbolic role and those who hold it). The hybrid reduces both forms' extractiveness but introduces ambiguity costs — the boundary between ceremonial and political authority is permanently contestable, and disputes require constitutional interpretation (theater grows from 0.35 to 0.44, then stabilizes). Suppression is also moderate (0.38) because the arrangement must suppress both pure-form advocates without appearing to suppress their legitimate concerns — the constitutional text formally acknowledges both sources of legitimacy, so overt suppression of either camp would delegitimize the hybrid itself. The theater ratio's rise and stabilization reflects the increasing intensity of constitutional boundary-maintenance rhetoric as the arrangement matures and both camps learn to frame their objections in hybrid terms. Measurements are on a single shared time grid (every metric authored at every time point) so temporal analysis is alignment-safe.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (monarch and elected officials) perceive the arrangement as genuinely coordinative: it grounds their respective powers in different legitimate sources and prevents either from monopolizing. The victim seats (absolutist and republican camps) perceive the same arrangement as extractive: each believes the other's source of legitimacy is spurious, so the hybrid appears to them as enforced compromise that constrains the 'true' form. The constitutional interpreters occupy an observer seat that is structurally central — they define the boundary the whole arrangement depends on, so their decisions materially affect which camp feels more constrained at any moment. The engine computes this divergence from the structural data: the monarch's identity-locked status and retained veto powers position them as a beneficiary; the absolutist and republican camps' structural exclusion from prevailing in their pure-form demands positions them as payers. The hybrid reading itself is a third-order seat — those committed to it as the legitimate endpoint are neither pure-form advocates nor beneficiaries of the compromise, but architects of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the hereditary monarch is low (d near 0.2–0.3), marking them as a beneficiary: they retain status, income, and veto powers without the cost of governing. Their exit is impossible (identity-locked), which initially suggests high d, but the constitutional framework explicitly protects their role, so the constraint subsidizes them. The elected officials are also beneficiaries (d near 0.3–0.4): they exercise substantial authority delegated constitutionally, their power is grounded in explicit popular consent, and they can exit via electoral defeat. Citizenry are near-symmetric (d near 0.45–0.55): they receive genuine coordination benefit (stable state, no civil war over legitimacy) but pay through maintaining the dual apparatus and constraint on alternatives. Absolutist monarchists are targets (d near 0.65–0.75): they are constitutionally barred from their preferred form, their exit is constrained (they cannot abolish elections), and they bear the cost of living in a system they believe illegitimate. Republican democrats are also targets (d near 0.60–0.70) but with slightly lower d because they have more global institutional allies (other republics) and can advocate constitutional amendment (mobile exit is more realistic for them than for monarchists); however, they are still structurally constrained by the constitutional text that encodes the hybrid. Constitutional interpreters are analytical (exit by removing themselves from the interpretive role; they do not collect from the constraint directly).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and materially different from the constraint itself: the problem is 'how can both inherited and delegated legitimacy coexist,' and the constraint solves it by constitutional separation and interpretation. However, the solution introduces a new problem: who adjudicates the boundary? If constitutional interpretation becomes the seat of new extraction (if interpreters systematically favor one camp), the constraint converts from tangled-rope (genuine coordination with asymmetric benefit) into snare (pure extraction using coordination as cover). The theater ratio's growth (from 0.35 to 0.44) suggests this risk is present — as the boundary becomes more frequently contested, the proportion of enforcement activity devoted to *maintaining the boundary itself* grows, rather than to delivering the original coordination function. Mandatrophy is not yet resolved (the constraint is still functionally coordinative and still solves the founding problem), but the measurement trajectory indicates vulnerability: if the theater ratio continues rising above 0.50, and if extractiveness shifts upward toward 0.55+, the classification would shift toward piton (atrophied coordination, maintained theatrically). The constraint currently avoids mandatrophy by the fact that both beneficiary seats (monarch and elected officials) have incentive to maintain it and neither has abandoned the arrangement. But if republicanism ever prevails constitutionally (as it did in Ireland, Italy), the constraint dissolves entirely — not into mandatrophy but into supersession by a new constraint (pure republican legitimacy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_irresolvable,
    'Can the constitutional hybrid reading coexist indefinitely with monarchical and republican readings, or does one reading eventually foreclose the others?',
    'Historical analysis of constitutional amendments and regime transitions: do states that adopt the hybrid ever revert to pure monarchy or pure republicanism, and when they do, is it because the hybrid was inherently unstable or because external political pressure forced the transition?',
    'If the hybrid is inherently stable, the three readings coexist_with each other indefinitely. If historical regimes show a pattern of hybrid collapse into one of the pure forms, the readings are mutually foreclose (each reading is trying to prevent the others from monopolizing). This affects whether the constraint is properly classified as tangled_rope (genuine long-term compromise) or snare (unstable compromise maintained only while no faction gains sufficient power to impose its pure form).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_irresolvable, empirical, 'Stability of the constitutional hybrid against conversion to pure monarchy or pure republicanism.').

omega_variable(
    boundary_interpretation_drift,
    'Does constitutional interpretation of the ceremonial/political boundary systematically favor one camp (monarchists or republicans) over time, or does it oscillate?',
    'Analysis of landmark constitutional court decisions: do courts tend to expand or contract the monarch''s powers, or does the boundary remain stable? If they expand or contract, whose interests benefit?',
    'If boundary interpretation drifts consistently in one direction (say, republicanward — narrowing the monarch''s effective powers), the constraint converts functionally into pure republicanism even though the monarchist form remains on paper (theater_ratio becomes the primary metric). If interpretation oscillates, the constraint remains tangled_rope. If interpretation is stable, the constraint''s stability is genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_interpretation_drift, empirical, 'Whether constitutional interpretation of the hybrid boundary exhibits directional drift favoring one pure form.').

omega_variable(
    ceremonial_power_vs_symbolic_authority,
    'Is the monarch''s ''ceremonial authority'' merely symbolic (holding no material power) or does it include veto rights, treaty-signature authority, or appointment powers that constitute real political leverage?',
    'Comparison of different constitutional monarchies: UK (assent required for legislation, ceremonial appointment powers), Spain (veto and treaty powers more limited), Netherlands (mostly ceremonial). Do states with more substantive ceremonial powers experience different extraction profiles than states with purely symbolic monarchs?',
    'If ceremonial authority is purely symbolic, the monarch is a beneficiary collecting income and status with no real power, making the arrangement closer to pure republicanism with a luxury figurehead. If ceremonial authority includes real powers, the monarch is a co-ruler, making the arrangement closer to genuine power-sharing and more defensible as true compromise. This affects whether ε should be lower (genuine coordination) or higher (one camp is subordinated despite retaining offices).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ceremonial_power_vs_symbolic_authority, empirical, 'Whether the constitutional hybrid''s ceremonial powers are substantive or purely symbolic.').

omega_variable(
    alternative_readings_foreclosure,
    'Does the constitutional hybrid reading logically foreclose the monarchical and republican readings within the same state''s framework, or can all three coexist as live positions held by different political factions?',
    'Political discourse analysis: do monarchists and republicans in hybrid constitutional monarchies argue that the hybrid is illegitimate and their pure form is the true endpoint, or do they accept the hybrid as legitimate even while preferring their own?',
    'If the readings are mutually foreclosing, the constraint operates as suppression of two live alternatives by a third. If they coexist_with each other as live positions, the constraint is managing genuine pluralism. This affects the classification: suppression of live alternatives without their consent = snare-tendency; management of genuine pluralism = rope-tendency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_foreclosure, conceptual, 'Whether the hybrid reading forecloses or coexists with pure-form readings as live political claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(sove_tr_t0, observed).
narrative_ontology:measurement(sove_tr_t5, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(sove_tr_t5, observed).
narrative_ontology:measurement(sove_tr_t10, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(sove_tr_t10, observed).
narrative_ontology:measurement(sove_tr_t15, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement_basis(sove_tr_t15, observed).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(sove_tr_t20, observed).
narrative_ontology:measurement(sove_tr_t25, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(sove_tr_t25, observed).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement_basis(sove_tr_t30, observed).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(sove_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(sove_be_t0, observed).
narrative_ontology:measurement(sove_be_t5, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 5, 0.39).
narrative_ontology:measurement_basis(sove_be_t5, observed).
narrative_ontology:measurement(sove_be_t10, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement_basis(sove_be_t10, observed).
narrative_ontology:measurement(sove_be_t15, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(sove_be_t15, observed).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement_basis(sove_be_t20, observed).
narrative_ontology:measurement(sove_be_t25, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(sove_be_t25, observed).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement_basis(sove_be_t30, observed).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(sove_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(sove_su_t0, observed).
narrative_ontology:measurement(sove_su_t5, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement_basis(sove_su_t5, observed).
narrative_ontology:measurement(sove_su_t10, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement_basis(sove_su_t10, observed).
narrative_ontology:measurement(sove_su_t15, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(sove_su_t15, observed).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(sove_su_t20, observed).
narrative_ontology:measurement(sove_su_t25, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 25, 0.39).
narrative_ontology:measurement_basis(sove_su_t25, observed).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(sove_su_t30, observed).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(sove_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__constitutional_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__republican_reading).

% DUAL FORMULATION NOTE:
% The constraint 'sovereign_legitimacy' decomposes into three structurally distinct readings: (1) monarchical_reading (ε high for republicans, low for monarchists; pure downward-flow reading), (2) republican_reading (ε high for monarchists, low for republicans; pure upward-flow reading), (3) constitutional_hybrid_reading (ε moderate for both camps; compromise reading). The three readings share a kernel (the question of authority's source) but instantiate different constraints with different ε values, different beneficiary/victim structures, and different persistence mechanisms. This story is the constitutional_hybrid_reading. The network links show causal influence: the hybrid reading's stability depends on preventing either pure form from prevailing, so it affects the practical possibility of the monarchical and republican readings being instantiated as governing arrangements. Each sibling reading is a separate constraint file with its own stakeholders, measurements, and claim/metric structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__constitutional_hybrid_reading, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
