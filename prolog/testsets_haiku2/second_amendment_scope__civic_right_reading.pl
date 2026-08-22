% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment Individual Right Conditioned on Civic Militia Participation
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested Second
 *   Amendment kernel: the right to bear arms is an INDIVIDUAL right, but it
 *   is legitimated and conditioned by CIVIC MILITIA PARTICIPATION. Unlike the
 *   pure individual-right reading (severing militia from protection), this
 *   reading keeps the militia condition structurally central—only persons
 *   meeting militia-eligibility criteria (citizenship, age, training, service
 *   readiness, or non-criminal status) retain protected status. Unlike the
 *   collective-right reading (making the right inhere in state authority
 *   alone), this reading locates the protected interest in the individual
 *   subject to the participation gate. The constraint thus COORDINATES civic
 *   participation with constitutional status: it solves the founding-era
 *   problem of how to ground an individual right in collective duty rather
 *   than pure personal preference. The measurement series projects modest
 *   upward drift in extractiveness (0.35→0.42 over 50 years) as regulatory
 *   pressure on non-qualifying populations increases and
 *   militia-participation criteria become more formalized and restrictive in
 *   practice, while theater ratio remains modest (0.18→0.22, then stable)
 *   because the interpretive work required to maintain the militia-condition
 *   reading grows as social practice diverges from civic participation norms.
 *
 * KEY AGENTS:
 *   - Militia-eligible citizens: constitutional beneficiaries under this reading; retain protected right when they maintain eligibility criteria
 *   - Non-militia-eligible persons (felons, non-citizens, conscientious objectors): bear the cost of the gating mechanism; excluded from protection
 *   - Regulatory authorities (state and federal): constrained in their authority to regulate firearms for militia-eligible persons; retain authority for excluded populations
 *   - Interpretive judiciary (Supreme Court, appellate courts): set and enforce the meaning of militia-condition reading; their ongoing work maintains the constraint
 *   - Public safety advocates: excluded from the constitutional reading; cannot implement preferred policies for protected populations without constitutional collision
 *   - Militia organizations: benefit from the constitutional framework that grounds their members' rights and their social role
 *   - Gun manufacturers: benefit from a protected market, though narrower than unconditional individual-right reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.42).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.38).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment Individual Right Conditioned on Civic Militia Participation").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f').
narrative_ontology:cs_kernel_codification('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', fixed_text).
narrative_ontology:cs_authority_grounding('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', lineage).
narrative_ontology:cs_interpretation_layer_present('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f').
narrative_ontology:cs_reading_relation('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', foundational, individual_right_grounded_in_civic_participation).
narrative_ontology:cs_axiom_status(individual_right_grounded_in_civic_participation, holdable).
narrative_ontology:cs_axiom_grounding('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', individual_right_grounded_in_civic_participation, deontological).
narrative_ontology:cs_axiom('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', foundational, militia_condition_constitutive_of_protected_status).
narrative_ontology:cs_axiom_status(militia_condition_constitutive_of_protected_status, holdable).
narrative_ontology:cs_axiom_grounding('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', militia_condition_constitutive_of_protected_status, conventional).
narrative_ontology:cs_reference_frame('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', civic_militia_framing_founding_era).
narrative_ontology:cs_drift_state('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', contemporary_professionalized_military, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('940d4fdc-e661-4bdd-9cb3-2fd2fe60a84f', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_militia_eligible_persons).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, regulatory_authorities_constrained).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_organizations_civic).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, gun_manufacturers).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republicanism_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, armed_citizenship_participation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals meeting civic participation criteria (age, citizenship, training, militia service or readiness) retain protected right to firearm ownership. The constraint frames their participation in civic defense infrastructure as the condition precedent for the right. They benefit from constitutional protection and from the social legitimacy that militia-grounding provides. They can exit by failing to maintain militia eligibility, though most do not.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    organized, generational, mobile, national).

% Persons who cannot or do not participate in civic militia structure (felons, non-citizens, conscientious objectors, those below military age) are excluded from Second Amendment protection under this reading. They bear the cost of the constraint's gating mechanism—their right claim is not recognized. They cannot practically exit this status without changing fundamental legal position.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, non_militia_eligible_persons, payer,
    powerless, biographical, trapped, national).

% State and federal regulatory bodies are constrained in firearm regulation by the constitutional right for militia-eligible persons. They cannot impose blanket prohibitions on qualifying citizens without running the Second Amendment gauntlet. The reading's condition on militia participation modulates their regulatory space—they retain authority over non-qualified persons but face constitutional limits for qualified ones.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, regulatory_authorities_constrained, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, regulatory_authorities_constrained, agenda_setter).

% Courts, particularly the U.S. Supreme Court, set and enforce the meaning of the Second Amendment. Under this reading, they establish and maintain the criteria for militia eligibility, the scope of protected conduct, and the permissible regulations on non-qualifying populations. Their ongoing interpretive work is what holds the constraint in place.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, interpretive_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Organizations and advocates prioritizing firearm injury reduction would argue for broader regulatory authority unconditioned on militia status. They are substantially excluded from the constitutional reading; their preferred policy alternatives (universal background checks, red-flag laws, ownership prohibitions) cannot be implemented for militia-eligible persons without constitutional collision.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, public_safety_advocates, excluded,
    moderate, biographical, constrained, national).

% Organized militia bodies (state guards, civil defense groups, civic associations meeting militia participation standards) benefit from the constitutional framework that grounds their members' firearm rights and their own organizational legitimacy. The constraint vindicates their social role as the condition of civic protection rights.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_organizations_civic, beneficiary,
    moderate, generational, mobile, national).

% Firearm manufacturers benefit from a protected market of militia-eligible purchasers, though this reading is narrower than an unconditional individual-right reading would be. They have substantial exit options (e.g., shifting markets internationally) but remain heavily invested in U.S. domestic rights frameworks. They are primary observers of the constitutional stability this reading provides.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, gun_manufacturers, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, gun_manufacturers, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__civic_right_reading, interpretive_judiciary).
narrative_ontology:fixing_cost_class(second_amendment_scope__civic_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates civic defense participation with constitutional protection: frames the right to bear arms as inseparable from participation in organized militia structures, solving the problem of how to ground an individual right in collective civic duty rather than pure personal choice.
% TRANSFER_FUNCTION: Transfers constitutional legitimacy and legal protection to militia-eligible persons while transferring regulatory constraints and exclusion to non-qualifying populations. Moves political authority over firearm policy toward the interpretive judiciary and away from unconstrained legislative regulation.
% ABSENT_VOICES: Non-militia-eligible persons cannot participate in the constitutional negotiation that frames the right; their interests are represented, if at all, only by public-safety advocates and regulatory authorities, who themselves are largely excluded from canonical Second Amendment interpretation. Persons with felony records, non-citizens, and pacifists have no seat in the framework despite bearing its costs.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared and the Second Amendment were read as either a pure individual right (severed from militia) or only collective state authority, the regulatory and constitutional landscape would reorganize significantly. Firearm regulation would shift toward either much broader protection or much broader restriction; the militia-grounding logic would no longer anchor the right, and the interpretive space for new regulations would expand.
% FOUNDING_PROBLEM: The Framers of the Second Amendment sought to preserve the capacity for armed citizenry to participate in civic defense without standing armies dominating the political order, while preventing pure mercenary power from concentrating in a professional military. The militia condition was meant to distinguish protected arms-bearing (civic duty) from private violence.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of original public meaning (Rakove, Amar) and civic republicanism theorists (Pocock, Bailyn) attest to founding-era militia-grounding as historically documented. Gun-rights advocates and the Supreme Court majority in District of Columbia v. Heller (2008) dispute whether militia conditioning survives contemporary constitutional reading. Historians outside the contemporary policy debate largely confirm the founding-era framing while noting the interpretive distance to modern practice.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).
:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading is CLASSIFIED AS TANGLED ROPE because it possesses BOTH genuine coordination (civic participation and constitutional protection are woven together to solve a founding-era problem) AND asymmetric extraction (non-qualifying persons bear the cost of gating without participation benefit; regulatory authorities are constrained). Extractiveness is MODERATE (0.42) because the constraint does coordinate real civic structures (militia, state defense participation) with constitutional status—some of the 'extraction' is actually the cost of that coordination. But it also extracts from the excluded population (non-qualifying persons have no say in the criteria that bar them; felons bear permanent exclusion). Suppression is MODERATE (0.38) because: (1) the constraint's persistence depends on continued judicial enforcement of the militia-condition reading—this is active work, not natural law, and if the judiciary shifted to pure individual-right reading the constraint would collapse; (2) exit options for militia-eligible persons are MOBILE—they can choose not to maintain militia service and thereby lose protection, though most do not; (3) the excluded population faces TRAPPED exit (changing felony status or citizenship is not practical exit). Theater ratio is LOW-MODERATE (0.22) because much of the interpretive work is genuine constitutional analysis, but some is performative maintenance of the militia-gating function as social practice drifts away from civic militia participation. ACCESSIBILITY COLLAPSE is MODERATE-HIGH (0.65) because once the militia-conditioning frame is established in law, alternatives (pure individual-right, pure collective-right) are partly foreclosed—the frame generates its own institutional inertia and judicial precedent. But alternatives remain live in scholarly and political discourse, so collapse is not complete. RESISTANCE is MODERATE-HIGH (0.58) because substantial communities (public-safety advocates, non-militia-eligible persons, originalist scholars who read the founding intent differently) actively resist this reading; gun-rights advocates and militia organizations actively defend it. The constraint is contested at every interpretive level. THE CLAIM/METRIC INDEPENDENCE RULE: tangled_rope is claimed because the structural analysis shows both coordination and asymmetric extraction. The metrics are authored independently as descriptively true of this reading's operation. They will not be 'reconciled' to the claim—divergence is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   THE MILITIA-ELIGIBLE CITIZEN SEAT (beneficiary, organized power, generational time horizon, mobile exit): perceives the constraint as legitimate constitutional protection grounded in their civic duty; the militia-participation condition feels like a proper constitutional principle, not extraction. From their seat, the constraint IS coordination—I keep myself militia-ready and the Constitution protects my right to arms. REGULATORY AUTHORITY SEAT (agenda-setter, institutional power, generational time horizon, constrained exit): perceives the constraint as a limit on their authority. They cannot impose blanket regulations on protected populations; they experience the constraint as asymmetric—they must enforce gating for non-qualifying persons while respecting protection for qualifying ones. From their seat, the constraint limits their options. NON-MILITIA-ELIGIBLE PERSON SEAT (payer, powerless, biographical time horizon, trapped exit): perceives the constraint as arbitrary exclusion. The criteria for militia eligibility are not choices they made; they lack political power to change them. They bear the cost of regulation (restricted firearm access) while gaining no constitutional protection. The constraint feels like pure extraction—their exclusion serves no benefit to them. THE ENGINE COMPUTES THESE DIVERGENCES from the structural data. The beneficiary seat has LOW directionality (d near 0.0—cost is low, benefit is high, exit is voluntary). The regulatory authority seat has MIXED directionality (constrained exit, but they retain authority over part of the population). The payer seat has HIGH directionality (d near 1.0—cost is high, exit is trapped, benefit is nil). These divergences are not errors; they are the point of per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   MILITIA-ELIGIBLE CITIZENS derive LOW directionality (d ≈ 0.15–0.25): they are primary beneficiaries (constitutional protection for themselves), hold organized power (can form militia groups, advocate collectively), and have mobile exit options (they can decline militia participation if they choose, though most do not). The beneficiary declaration points them toward d=0. Structural derivation: beneficiary + organized + mobile exit → d well below 0.5. REGULATORY AUTHORITIES derive MIXED-TO-HIGH directionality (d ≈ 0.55–0.65): they are neither pure beneficiary nor pure target. They SET the constraint (agenda-setter role) but are also CONSTRAINED BY IT (victim role). They retain authority over non-qualifying populations (benefit), but lose authority over qualifying ones (cost). Their exit options are constrained—they cannot simply abandon constitutional interpretation. Structural derivation: agenda-setter with constraints + institutional power + constrained exit → d near 0.5–0.6. This is not an override; the structural data produces mixed directionality. NON-MILITIA-ELIGIBLE PERSONS derive HIGH directionality (d ≈ 0.80–0.90): they are pure targets (excluded from protection), hold powerless position (no organized capacity to change the rule), and have trapped exit options (changing felony status or citizenship is not realistic exit). Victim declaration + powerless + trapped exit → d near 1.0. MILITIA ORGANIZATIONS and GUN MANUFACTURERS derive LOW directionality (d ≈ 0.10–0.20): they are beneficiaries (protected market, constitutional vindication), hold powerful or organized positions, and have arbitrage or mobile exit options. No overrides are warranted; structural derivation produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   THE CONSTRAINT'S MANDATE is: 'Preserve the capacity for armed citizenry to participate in civic defense without standing armies dominating political order, AND distinguish protected arms-bearing (civic duty) from private violence.' MANDATE STATUS: The first component (armed citizenry, civic defense, avoiding standing-army dominance) is LIVE and persistent—the civic militia ideal remains part of constitutional culture. The second component (distinguishing civic arms from private violence via militia condition) is INCREASINGLY DEAD in practice. Modern militia participation is not the norm; background checks and registration are now standard (militia-eligibility criteria have been formalized into bureaucratic gates far removed from civic participation). The constraint PERSISTS despite mandate atrophy. THE MANDATROPHY SIGNAL: Under this reading, the constraint's persistence depends on the judiciary continuing to maintain the militia-condition interpretation even as actual militia participation declines and the founding problem (standing armies dominating politics) has been solved by professional military subordination to civilian command rather than by armed citizenry checks. If the mandate dies completely (civic militia is no longer seen as necessary to resist tyranny), the constraint becomes PITON-LIKE—maintained by theater and institutional inertia rather than functional necessity. CURRENT STATE: The constraint is TANGLED ROPE with MANDATROPHY PRESSURE (not yet resolved into pure piton, but the functional gap is widening). The interpretive work required to maintain the militia-condition reading grows as the founding problem recedes and civic participation diverges from modern reality. This is why theater ratio is projected to rise modestly over 50 years (0.18→0.22, then stabilize)—as mandatrophy pressure increases, the interpretive work becomes more performative (justifying why militia condition is still relevant when it is less so) and less functional (the condition no longer solves the founding problem in contemporary practice).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_eligibility_criteria_drift,
    'How are ''militia-eligible citizens'' defined and operationalized in contemporary law, and does the list of qualifying criteria remain coherent with the founding-era civic participation concept?',
    'Empirical audit of state militia statutes, federal firearm licensing criteria, and court precedent establishing eligibility thresholds; comparison with founding-era militia rosters and practice.',
    'If eligibility criteria become increasingly formalized and disconnected from actual militia participation (e.g., mere age and citizenship with no service requirement), the constraint drifts toward pure individual-right reading even as the reading label remains ''civic.'' Conversely, if modern militia organizations successfully reinstitute active participation requirements, the civic framing becomes more operationally real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_eligibility_criteria_drift, empirical, 'Whether militia-eligibility operationalization remains coherent with civic participation intent.').

omega_variable(
    founding_problem_persistence,
    'Does the founding problem (armed citizenry needed to resist standing-army tyranny) remain structurally live in contemporary governance, or has it been solved by professional military subordination to civilian command?',
    'Comparison of founding-era concerns (standing armies as tyranny threat) with contemporary civil-military relations; examination of whether civic-armed capacity materially constrains military options in ways civilian command does not.',
    'If the founding problem is solved, the militia-condition reading becomes increasingly mandatrophic—it persists as theater and institutional inertia rather than functional necessity. If the problem remains live (e.g., in contexts of state collapse or democratic failure), the reading retains functional legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, conceptual, 'Whether the founding-era threat (standing-army tyranny) remains structurally live or has been resolved by alternative mechanisms.').

omega_variable(
    reading_foreclosure_asymmetry,
    'Does the civic-right reading logically foreclose the pure individual-right reading, or can a party coherently hold both simultaneously?',
    'Formal analysis of the logical relationship between ''individual right conditioned on militia'' and ''individual right unconditioned.'' Can a constitutional order recognize individual-right protection for militia-eligible persons while denying it for non-qualifying persons, or does the individual-right concept demand universality?',
    'If the readings foreclose each other, the constraint field shows binary contest; if they coexist, the constraint field shows pluralistic coexistence (different parties in the same regime holding different readings). This affects the classification of reading_relations from coexists_with to forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_asymmetry, conceptual, 'Whether the civic-right reading and pure individual-right reading logically foreclose each other or can coexist.').

omega_variable(
    excluded_population_substitution_capacity,
    'Can non-militia-eligible persons (felons, non-citizens, conscientious objectors) organize politically to change militia-eligibility criteria, or are they structurally excluded from the constitutional negotiation itself?',
    'Political-process analysis: do non-qualifying populations have advocacy channels (legislative, judicial, electoral) through which they can contest their exclusion, or are they locked out by design?',
    'High substitution capacity would lower suppression and raise accessibility collapse (alternatives become available if political power coalesces); low capacity would raise suppression (structural lock-in) and indicate the constraint approaches pure snare for the excluded population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_population_substitution_capacity, empirical, 'Whether excluded populations can organize to contest their exclusion or are structurally locked out.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(seco_tr_t0, projected).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__civic_right_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(seco_tr_t10, projected).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__civic_right_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(seco_tr_t20, projected).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__civic_right_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(seco_tr_t30, projected).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__civic_right_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(seco_tr_t40, projected).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__civic_right_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(seco_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(seco_be_t0, projected).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__civic_right_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement_basis(seco_be_t10, projected).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__civic_right_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(seco_be_t20, projected).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__civic_right_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement_basis(seco_be_t30, projected).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__civic_right_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement_basis(seco_be_t40, projected).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__civic_right_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement_basis(seco_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(seco_su_t0, projected).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__civic_right_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(seco_su_t10, projected).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__civic_right_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(seco_su_t20, projected).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__civic_right_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement_basis(seco_su_t30, projected).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__civic_right_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(seco_su_t40, projected).
narrative_ontology:measurement(seco_su_t50, second_amendment_scope__civic_right_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement_basis(seco_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__civic_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% Second Amendment scope kernel decomposed into three structurally distinct readings (DP-001 ε-invariance). This file (civic_right_reading) represents the reading that conditions individual right on militia participation. Sibling readings (individual_right_reading: unconditioned individual right; collective_right_reading: state authority only) have different beneficiary/victim sets, different ε values, and different classification paths. Each is a separate constraint story. The reading_relations in cs_structure declare the logical relationships between readings (forecloses, coexists_with, influences). Network edges link all three stories so the corpus captures the contested kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
