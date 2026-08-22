% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: Charter Mandate: Secular Democratic Institutions with Civilian Military Subordination
 *   domain: constitutional/political/institutional
 *
 * SUMMARY:
 *   A post-revolutionary charter establishes secular democratic institutions
 *   with explicit military subordination to civilian authority. The charter
 *   is read by competing parties as instantiating different foundational
 *   commitments: the secular democratic reading grounds legitimacy in
 *   constitutional process and electoral mandate; the military-custodian
 *   reading embeds military institutional autonomy as permanent guardian of
 *   stability; the guided-nationalism reading asserts Islamic identity as the
 *   sovereign foundation with democratic process as instrument. This story
 *   generates the secular-democratic reading as a single ε-invariant
 *   constraint. The reading benefits secular elites, urban professionals, and
 *   civil rights advocates (who gain institutional authority and rights
 *   protections) while extracting from military autonomy, political Islam
 *   actors, and rural religious communities (who lose autonomous authority or
 *   participation parity). The constraint is substantially extractive (0.68
 *   base ε) because it does not merely coordinate around shared values but
 *   actively suppresses alternative legitimacy grounds, enforced through
 *   constitutional doctrine, military chain-of-command reform, and legal bans
 *   on theocratic party organization.
 *
 * KEY AGENTS:
 *   - Secular democratic elite (agenda-setter, institutional power, arbitrage exit): draft and champion the charter, control constitutional interpretation, mobilize international democratic support.
 *   - Military officer corps (payer, powerful, identity-locked): bear subordination as loss of budgetary/foreign-policy autonomy and prestige; institutional identity fused to hierarchy makes exit impossible.
 *   - Political Islam actors (victim, organized, constrained exit): structurally excluded from exclusive authority; theocratic representation ruled out; can participate in secular processes but not organize around religious legitimacy.
 *   - Urban professional class (beneficiary, organized, constrained exit): benefit from secular meritocratic institutions; dependent on institutional infrastructure so exit is constrained.
 *   - Civil rights advocates (beneficiary, moderate, mobile exit): defend the secular democratic reading against military and religious-nationalist reinterpretation; mobilize media and academia.
 *   - Rural religious communities (excluded, powerless, trapped): nominally in polity but experience displacement of traditional authority; no channels to articulate objections; geographically and economically entrapped.
 *   - Constitutional court (observer, institutional, analytical): interprets mandate; adjudicates competing readings through litigation; subject to political pressure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.72).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "Charter Mandate: Secular Democratic Institutions with Civilian Military Subordination").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional/political/institutional").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'efde8793-660a-43d5-9e9a-dd2db04699b2').
narrative_ontology:cs_kernel_codification('efde8793-660a-43d5-9e9a-dd2db04699b2', formalized).
narrative_ontology:cs_authority_grounding('efde8793-660a-43d5-9e9a-dd2db04699b2', lineage).
narrative_ontology:cs_interpretation_layer_present('efde8793-660a-43d5-9e9a-dd2db04699b2').
narrative_ontology:cs_reading_relation('efde8793-660a-43d5-9e9a-dd2db04699b2', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('efde8793-660a-43d5-9e9a-dd2db04699b2', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('efde8793-660a-43d5-9e9a-dd2db04699b2', foundational, secular_institutional_legitimacy).
narrative_ontology:cs_axiom_status(secular_institutional_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('efde8793-660a-43d5-9e9a-dd2db04699b2', secular_institutional_legitimacy, conventional).
narrative_ontology:cs_axiom('efde8793-660a-43d5-9e9a-dd2db04699b2', foundational, military_subordination_to_civilian_authority).
narrative_ontology:cs_axiom_status(military_subordination_to_civilian_authority, holdable).
narrative_ontology:cs_axiom_grounding('efde8793-660a-43d5-9e9a-dd2db04699b2', military_subordination_to_civilian_authority, deontological).
narrative_ontology:cs_reference_frame('efde8793-660a-43d5-9e9a-dd2db04699b2', secular_democratic_institutional_authority).
narrative_ontology:cs_drift_state('efde8793-660a-43d5-9e9a-dd2db04699b2', contemporary_constitutional_reinterpretation_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('efde8793-660a-43d5-9e9a-dd2db04699b2', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_elite).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, urban_professional_class).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institutional_autonomy).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_actors).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_officer_corps).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and champions the charter mandate establishing secular democratic institutions and civilian control of the military. They interpret the mandate as binding the military to executive and legislative authority, excluding religious-nationalist and military-custodian readings from legitimate constitutional standing. They author constitutional doctrine and control initial interpretation channels.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% Bears the constraint as subordination to civilian authority structures they view as weaker or fragmented. Professional identity and institutional prestige are fused to autonomy and hierarchy; the mandate strips them of independent budgetary authority, foreign policy voice, and emergency-powers claims. Exit options are severely constrained by professional embedding; they cannot leave without ceasing to be officers. They contest the reading privately and through institutional resistance.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_officer_corps, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, military_officer_corps, excluded).

% Included in the polity under the secular democratic mandate but structurally disadvantaged: theocratic representation is ruled out, religious law is subordinated to secular constitutional frameworks, and their core organizational logic (Islam as political legitimacy ground) is declared illegitimate. They can participate in secular democratic processes but cannot organize around their primary truth claim. Jamaat-e-Islami and allied movements face legal barriers to operating as religious parties.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_actors, payer,
    organized, biographical, constrained, national).

% Benefits from secular institutional frameworks (rule of law, professional certification, technocratic meritocracy) that the mandate establishes. Career paths open under secular law and professional standards; they have incentive to defend the mandate's institutional architecture but lack direct enforcement power. Their constraint exit options are limited by economic dependence on institutional infrastructure.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, urban_professional_class, beneficiary,
    organized, biographical, constrained, national).

% Champion the secular democratic mandate as ground for individual rights protections, freedom of conscience, and gender equality. They mobilize public discourse to defend the reading against military and religious-nationalist reinterpretation. Their power is diffuse (civil society, media, academia) and their exit options are relatively mobile (organizational defection, relocation), but they depend on the mandate's institutional anchoring to maintain legitimacy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_rights_advocates, beneficiary,
    moderate, biographical, mobile, national).

% Are nominally included in the secular polity but experience the mandate as displacement of traditional religious authority structures that have governed local life. Their organizational capacity runs through mosques and religious networks declared suspect or constrained by the mandate. They cannot easily exit (geographic and economic entrenchment) and lack channels to articulate objections within the constitutional framework.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, rural_religious_communities, excluded,
    powerless, biographical, trapped, local).

% Interprets and applies the charter mandate. Functions as the seat where competing readings (secular-democratic, military-custodian, guided-nationalism) are adjudicated through litigation. Holds formal interpretive authority but is subject to political pressure from all parties. Decisions either reinforce the secular democratic reading or create space for reinterpretation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% Monitor the mandate's implementation and measure compliance with secular democratic norms. Provide external legitimacy for the reading through technical assistance, aid conditionality, and public recognition. Their authority is soft but reinforces the charter's democratic framing.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democratic_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_elite).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified democratic state in which military force serves civilian political authority, ensuring that governing legitimacy flows from secular constitutional process rather than military prerogative, religious revelation, or ethnic nationalism. The mandate solves the founding coordination problem: how to prevent military and religious actors from fragmenting state authority.
% TRANSFER_FUNCTION: Transfers military budgetary and foreign-policy autonomy from the military institutional hierarchy to civilian executive and legislative control. Transfers theocratic representation claims from religious authorities to secular democratic processes open to religious-identity parties but not religious governance. Transfers authority to interpret the state's founding legitimacy from religious or military actors to constitutional courts and civil society.
% ABSENT_VOICES: Military officers who would defend institutional autonomy are structurally weakened but not entirely excluded (they retain formal representation). Political Islam actors who would claim theocracy as legitimate are excluded from exclusive authority but retain minority participation rights. Rural religious authorities who would defend traditional local governance are entirely absent from the drafting and constitutional conversation.
% DISAPPEARANCE_RATIONALE: If the charter mandate dissolved overnight, the military would reassert autonomous budgetary and foreign-policy authority, theocratic political parties would mobilize for religious-state frameworks, and competing territorial and identity claims would fragment the state apparatus. The entire institutional scaffolding—professional civil service, secular law schools, gender equality protections, individual rights case law—would lose its legitimacy anchor.
% FOUNDING_PROBLEM: Post-revolutionary state required a unified framework preventing military rule, religious theocracy, and ethnic fragmentation; needed to establish where sovereign legitimacy derives (not from generals, not from revelation, but from democratic process).
% FOUNDING_PROBLEM_CORROBORATION: Secular constitutional scholars and civil rights organizations attest the founding problem remains live and the mandate is essential. Military historians and nationalist intellectuals attest the founding problem is misframed—they argue the real problem was preventing chaos, which only military hierarchy or religious order can solve. Rural and religious-community leaders, absent from formal constitutional discourse, would likely attest the founding problem was not theirs and the mandate imposes an alien frame.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The secular democratic reading is classified as tangled_rope because it solves a genuine coordination problem (unifying state authority and preventing fragmentation) WHILE extracting asymmetrically from military autonomy and theocratic legitimacy claims. The measurement series show extractiveness and suppression both rising from t=0 to t=15, then plateauing—indicating institutional consolidation phase followed by steady-state enforcement. Theater rises gradually (0.22 to 0.41), suggesting that as the reading becomes institutionalized, more energy is spent on ceremonial affirmation (constitutional celebrations, military loyalty rituals) relative to functional governance. The three readings are coexisting rather than foreclosing—the guided-nationalism reading persists in political rhetoric, the military-custodian reading lurks as latent military doctrine, and the secular-democratic reading holds formal constitutional authority. On a shared measurement grid, all metrics are authored at every time point: extractiveness at t={0,5,10,15,25,35,40}; suppression_requirement at the same points; theater_ratio at the same points. The bidirectional causal loop between civilian subordination and military suppression is modeled in the rising suppression_requirement curve: as the civilian mandate strengthens, military institutional resistance requires more active enforcement (expanded constitutional doctrine, military purges, expanded civilian control mechanisms).
 *
 * PERSPECTIVAL GAP:
 *   From the secular democratic elite and civil rights seat, the constraint is rational democratic governance and progress. From the military officer-corps seat, it is institutional degradation and loss of professional autonomy. From the political Islam seat, it is exclusion from legitimate representation. The engine computes per-seat classification from power + exit + beneficiary/victim: the secular elite and civil rights advocates derive low d (beneficiaries, institutional backing) so compute low χ and rope-flavored types; the military and political Islam seats derive high d (victims, suppressed autonomy) so compute high χ and snare-flavored types. The story-level claimed_type (tangled_rope) reflects the overall structure; individual seats will diverge in their computed types based on directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations feed the directionality chain as follows: secular_democratic_elite and urban_professional_class are beneficiaries (gain institutional authority and rights protections without running enforcement machinery) → d near 0.1–0.2 (full beneficiary end). military_officer_corps is a victim (loses autonomous authority) + identity_locked exit (cannot cease to be officer) → d near 0.85 (full target end). political_islam_actors are victims (excluded from theocratic representation) + constrained exit → d near 0.75. rural_religious_communities are excluded (outside the charter conversation entirely) + trapped + powerless → d would be near 1.0 if they were explicitly named in the constraint, but they are excluded stakeholders, not seated parties, so directionality does not apply to them. No overrides are needed; the structural derivation from beneficiary/victim + exit + power produces coherent d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (preventing military rule, theocracy, and ethnic fragmentation) is declared 'contested' rather than 'live' or 'dead' because the three readings dispute whether the problem is still active. Secular democratic elites attest it is live and the mandate is essential. Military and nationalist actors dispute the framing: they argue the real problem was chaos, and only military or religious hierarchy solves it. The measurement series show extractiveness plateauing at 0.68 from t=25 onward, suggesting the constraint has become institutionalized and is no longer consolidating—it is maintained as steady-state enforcement rather than actively expanding. Theater_ratio plateaus at 0.41, indicating that the performance component is stable—neither degrading (sign of decay toward piton) nor rising (sign of deception amplification). The constraint shows no mandatrophy signal: it remains functionally extractive and actively enforced. However, the latency of competing readings (military-custodian and guided-nationalism) creates vulnerability to reinterpretation during crisis or leadership change—the secular reading is not immune from being displaced by a sibling reading via constitutional amendment or military intervention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_interpersonal,
    'Among the military officer corps (identity_locked exit), is the suppression they experience structurally imposed (by civilian chain of command, reduced budgets, constrained authority) or substantially internalized (they believe subordination is legitimate, have fused professional identity to civilian control)?',
    'Post-exit trajectory: if retired officers continue to advocate military subordination and accept civilian authority, suppression is internalized; if they agitate for military autonomy and view subordination as illegitimate constraint, suppression is structural and not internalized.',
    'If internalized, the military officer corps may not require sustained enforcement to maintain subordination—compliance becomes voluntary. If structural, the constraint must maintain active enforcement (constitutional doctrine, monitoring, reduced military budgets) to keep the military subordinated, and any enforcement relaxation will trigger reassertion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_interpersonal, empirical, 'Military subordination: internalized belief vs. structural coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(july_tr_t35, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(july_be_t35, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(july_su_t35, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'july_charter_sovereign_legitimacy'. The secular-democratic reading establishes secular democratic institutions with military subordination and political Islam exclusion/constraint. Sibling readings (military-custodian and guided-nationalism) instantiate the same charter text under different interpretations, producing different constraints with different ε values, beneficiary/victim structures, and types. The three readings coexist as live positions held by different political parties; no reading forecloses another within a single framework. Family links: this reading influences both siblings by establishing secular institutional authority as the reference frame against which they mount challenges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__secular_democratic_reading, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
