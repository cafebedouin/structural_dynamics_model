% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: NSL as Sovereignty Restoration After Constitutional Crisis
 *   domain: constitutional/political/security
 *
 * SUMMARY:
 *   The National Security Law (NSL), enacted in response to 2019 unrest, is
 *   read by the CPG security authority as a legitimate constitutional
 *   restoration instrument: it provides legal framework for prosecuting
 *   violent subversion, restoring state capacity, and securing public order
 *   after institutional fragmentation. This is ONE reading of a contested
 *   kernel — the NSL legal text itself — that also admits
 *   democratic-enclosure and jurisdictional-capture readings. This
 *   sovereignty-restoration reading frames the constraint as asymmetric
 *   coordination (restored public order benefits the general population;
 *   security enforcement targets political opposition classified as security
 *   threats). The claim is Tangled Rope (coordination function + targeted
 *   extraction); the metrics describe moderate-to-high extraction with
 *   suppression plateauing after initial ramp, consistent with a law whose
 *   scope has stabilized on political opposition rather than continuing to
 *   expand.
 *
 * KEY AGENTS:
 *   - CPG Security Authority: The institutional agenda-setter interpreting and enforcing NSL; claims restoration mandate
 *   - Political Protesters: Identity-locked payers facing prosecution for speech and assembly
 *   - Opposition Activists: Organized payers experiencing criminalization of dissent
 *   - Civil Liberties Advocates: Constrained payers bearing legal defense costs
 *   - General Population: Mobile beneficiaries of restored public order, diffuse cost-bearers
 *   - International Observers: Excluded parties that would dispute security-threat classification
 *   - Legal Establishment: Split between enforcement and critique
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.58).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.72).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "NSL as Sovereignty Restoration After Constitutional Crisis").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional/political/security").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, 'cbfd6d2c-d12a-48d0-8958-cfdfddb1db12').
narrative_ontology:cs_kernel_codification('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', formalized).
narrative_ontology:cs_authority_grounding('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', lineage).
narrative_ontology:cs_interpretation_layer_present('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12').
narrative_ontology:cs_reading_relation('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', foundational, emergency_security_restores_constitutional_order).
narrative_ontology:cs_axiom_status(emergency_security_restores_constitutional_order, holdable).
narrative_ontology:cs_axiom_grounding('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', emergency_security_restores_constitutional_order, deontological).
narrative_ontology:cs_axiom('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', foundational, subversive_coordination_threat_is_live).
narrative_ontology:cs_axiom_status(subversive_coordination_threat_is_live, holdable).
narrative_ontology:cs_axiom_grounding('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', subversive_coordination_threat_is_live, empirically_contingent).
narrative_ontology:cs_reference_frame('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', pre_2019_institutional_constitutional_authority).
narrative_ontology:cs_drift_state('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', post_nsl_enforcement_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cbfd6d2c-d12a-48d0-8958-cfdfddb1db12', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, cpg_security_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, constitutional_order_vindication).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, political_protesters).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, opposition_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, civil_liberties_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, general_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces NSL. Interprets 2019 unrest as subversive threat requiring emergency legal framework. Views NSL as restoring constitutional order and public safety. Collects expanded prosecutorial authority, budgetary resources, and legitimacy from security mandate. Cannot exit security role without abandoning their institutional position.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, cpg_security_authority, agenda_setter,
    institutional, generational, trapped, national).

% Face prosecution under NSL for street-level protest organizing, marches, and speech. Their political identity is the grounds for legal jeopardy. Exit options: internal exile (stop protesting and become politically invisible, abandoning core self), external exile (leave jurisdiction), or capitulation. Identity exit is painful because protest organizing is central to their self-conception as political actors.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, political_protesters, payer,
    powerless, biographical, identity_locked, national).

% Organized opposition leadership faces prosecution for protest coordination, party activities classified as subversive, and public statements deemed seditious. Coalition costs rise due to legal exposure; members defect under enforcement pressure. Exit options exist (shift to legal party politics, electoral focus, international exile) but feel like defeat to movement identities. Alternatives are available but constrain their action repertoire.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, opposition_activists, payer,
    organized, biographical, constrained, national).

% Human rights lawyers, journalists documenting NSL enforcement, and NGO monitors face prosecution when their work is classified as supporting illegal organizations or spreading seditious information. Professional identity brings legal risk; they bear mounting defense costs and pressure to self-censor their investigative and advocacy work.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, civil_liberties_advocates, payer,
    moderate, biographical, constrained, national).

% Experiences restored public order and reduced street-level violence and disruption. NSL enforcement is not targeted at them unless they engage in political protest or opposition activity. They receive coordination benefit (stable public space, security) while bearing diffuse cost through state budget devoted to security enforcement. Exit option is migration if enforcement tightens beyond their tolerance; for most they are mobile enough to remain unless enforcement expands.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, general_population, beneficiary,
    powerless, biographical, mobile, national).

% UN human rights mechanisms, foreign governments, and transnational NGOs document NSL as violating civil liberties and disproportionately targeting political opposition. Their authority to adjudicate is structurally excluded by CPG's sovereignty claim; they are kept out of the interpretation frame by the assertion that NSL is internal constitutional matter not subject to external judgment.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_observers, excluded,
    institutional, generational, analytical, global).

% Judges, prosecutors, and bar associations operate within NSL framework. Some view it as legitimate restoration; others experience pressure to apply law expansively or face institutional consequences. The legal profession is split: enforcement-aligned judges see order restoration, civil-liberties-oriented judges and lawyers see scope creep; bar associations struggle between professional ethics and institutional pressure.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, legal_establishment, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, cpg_security_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restores unified state capacity to detect and disrupt violent subversion after 2019 unrest fragmented police and prosecutorial response. Solves the coordination problem of centralizing security threat assessment and legal response so that violent organizing cannot exploit jurisdictional gaps.
% TRANSFER_FUNCTION: Transfers authority to classify political activity as security threat from diffuse policing to centralized security apparatus; moves civil liberties protections (speech, assembly, association) from constitutional protections to conditional grants revocable by security classification; moves enforcement costs onto payers (prosecution, detention, legal defense).
% ABSENT_VOICES: Pro-democracy activists, international human rights bodies, and opposition legal scholars would argue NSL's definition of subversion encompasses lawful political opposition and that constitutional order cannot be restored by suspending constitutional protections. They are structurally excluded by the security-threat framing, which classifies their advocacy as subversive if voiced.
% DISAPPEARANCE_RATIONALE: If NSL enforcement ceased, political organizing would resume openly, opposition parties would rebuild their mobilizing capacity, street-level activism would reorganize, and the CPG security authority would lose prosecutorial basis for opposition targeting. Public order would shift from current suppressed-mobilization state to pre-NSL contested-street-presence state.
% FOUNDING_PROBLEM: 2019 unrest: mass street protests escalating to violence, institutional coordination breakdown, police response fragmentation, and what the CPG classified as coordinated subversion attempt requiring emergency legal framework to restore state authority and constitutional order.
% FOUNDING_PROBLEM_CORROBORATION: CPG security establishment and allied analysts attest 2019 unrest was organizationally coordinated, partially violent, and required emergency response. International observers and opposition parties attest violence was limited, contextually understandable in protest setting, and that NSL's scope far exceeds proportionate security response. Independent research documents protest composition and violence patterns; disagreement is located in interpretation of what they mean (security threat vs. rights-driven protest).
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness plateaus at 0.58 (moderate-high): it targets political opposition specifically rather than general population, so it is asymmetric but not maximal. The constraint directly extracts political voice; it is not attempting to extract economic resources. Suppression reaches 0.72 because the NSL's enforcement depends actively on detecting, arresting, and prosecuting political opponents — reducing resistance from that group is structural to the law's operation. Theater is moderate (0.41): the security review function is real (threat assessment exists; some prosecutions address actual coordination), but growing share of NSL enforcement addresses lawful political speech reclassified as subversive, not genuine security threats. The measurement series plateau after t=18, indicating enforcement intensity stabilized once the opposition capacity was significantly degraded.
 *
 * PERSPECTIVAL GAP:
 *   The CPG security authority views NSL as restoring constitutional order and protecting public safety — a coordination function that benefits society. Political protesters view it as permanent criminalization of their core identity; they experience maximal extraction (freedom of expression, assembly, association). This divergence is structural: the same legal text produces legitimate-restoration for the authority and political persecution for the opposition. The engine computes this divergence by deriving directionality from victim/beneficiary declarations and exit options: CPG sits at d near 0.0 (beneficiary, trapped in security role); protesters sit at d near 1.0 (victims, identity-locked escape).
 *
 * DIRECTIONALITY LOGIC:
 *   CPG security authority: beneficiary (collects expanded prosecutorial authority, vindicated constitutional interpretation), powerful institutional position, trapped exit (cannot abandon security mandate), derives d ≈ 0.15 (low/beneficiary). Political protesters: victims (face prosecution, surveillance), powerless, identity-locked exit (cannot renounce political identity without self-erasure), derives d ≈ 0.95 (high/target). Opposition activists: victims (criminalized organizing), organized but outmatched, constrained exit (can retreat to legal politics but that forecloses their action repertoire), derives d ≈ 0.88. General population: beneficiary (order restored, cost diffuse), powerless but mobile exit (can move jurisdictions if enforcement tightens beyond their tolerance), derives d ≈ 0.25 (low-moderate/beneficiary). Civil liberties advocates: victims (legal jeopardy), moderate power, constrained exit (professional identity at risk), derives d ≈ 0.72. No directionality overrides needed; structural derivation captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty-restoration reading avoids mandatrophy reclassification (snare vs. rope) by maintaining that the founding problem (2019 unrest) is live and that NSL is the mechanism for solving it. However, the reading is highly vulnerable to mandatrophy challenge: if the founding problem is actually resolved (protest capacity degraded, street violence ended), then NSL's continued enforcement against political opposition becomes rent-preservation, not constitutional restoration. The measurement plateau at t=18+ could signal mandatrophy onset if it reflects not achievement of the founding goal but shift to political-opposition-targeting after security threat is neutralized. The commentary documents the contested status of the founding problem precisely to enable this mandatrophy detection: if external observers can demonstrate 2019 unrest is 'dead' (the threat is gone), then NSL's continued operation against opposition parties is extracted authority, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_classification,
    'Was 2019 unrest primarily a security threat requiring emergency powers (CPG framing) or a rights-driven protest movement that was mislabeled as subversion?',
    'Independent research analyzing protest composition, violence patterns, and organizational structure; international human rights fact-finding; comparison with other jurisdictions'' protest-response models; longitudinal analysis of CPG prosecution records to determine what percentage targets violence vs. speech.',
    'If 2019 unrest was primarily rights-driven protest, the founding problem is mischaracterized and NSL is a snare targeting political opposition, not a coordination mechanism. If security threat was real and acute, NSL''s targeting of opposition remains extraction but can be defended as collateral cost of necessary security response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_classification, empirical, 'Classification of 2019 unrest as security threat vs. political protest').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (legal penalties, surveillance, arrest) or internalized (opposition members self-censoring, withdrawing from politics due to fear they carry)?',
    'Post-suppression behavioral analysis: if opposition capacity recovers quickly after NSL is loosened, suppression is mainly structural; if degradation persists long after legal threat recedes, suppression is partially internalized.',
    'High internalized suppression means the constraint is more extractive than the structural measure alone suggests — it carries forward constraint''s effects even after removal of enforcement mechanism. This would support reclassification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in NSL enforcement').

omega_variable(
    constitutional_order_definition,
    'Does ''constitutional order restoration'' mean restoring pre-2019 institutional arrangements (CPG reading) or restoring constitutional protections for speech/assembly that NSL itself suspends (opposition reading)?',
    'Analysis of constitutional text and prior jurisprudence; comparative constitutionalism on whether emergency provisions can themselves violate core constitutional commitments; examination of what constitutional order the drafting authority claimed to restore.',
    'If constitutional order includes protection for political speech and assembly, then NSL''s suspension of these protections is itself unconstitutional — the constraint is a snare masquerading as constitutional restoration. If constitutional order is read narrowly as territorial integrity and state authority, NSL is legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_order_definition, conceptual, 'Definition of constitutional order that NSL claims to restore').

omega_variable(
    sibling_reading_kernel_contest,
    'What kernel-level ambiguity produces the three distinct readings (sovereignty restoration vs. democratic enclosure vs. jurisdictional capture)?',
    'Textual analysis of NSL provisions that can be read either as security-proportionate or open-ended; examination of enforcement practice to determine if scope matches text or exceeds it; analysis of which institutional actor (CPG security authority vs. opposition parties vs. international observers) has authority to interpret NSL''s scope.',
    'The kernel contest is located in NSL''s text itself: broad definition of ''subversion'' and ''sedition'' that can encompass security threats or lawful opposition depending on enforcement choices. The readings are not equally true; they are differently adaptive to different power positions. This omega documents the irreducible textual under-determination that permits three readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_contest, conceptual, 'Kernel-level textual ambiguity permitting multiple readings of NSL').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_sov_restoration_tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nsl_sov_restoration_tr_t3, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(nsl_sov_restoration_tr_t6, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(nsl_sov_restoration_tr_t12, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(nsl_sov_restoration_tr_t18, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement(nsl_sov_restoration_tr_t25, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(nsl_sov_restoration_be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nsl_sov_restoration_be_t3, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(nsl_sov_restoration_be_t6, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(nsl_sov_restoration_be_t12, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(nsl_sov_restoration_be_t18, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(nsl_sov_restoration_be_t25, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(nsl_sov_restoration_su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(nsl_sov_restoration_su_t3, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 3, 0.61).
narrative_ontology:measurement(nsl_sov_restoration_su_t6, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(nsl_sov_restoration_su_t12, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(nsl_sov_restoration_su_t18, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(nsl_sov_restoration_su_t25, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__sovereignty_restoration_reading, 0.12).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% The NSL legal text kernel admits three distinct readings: sovereignty_restoration_reading (this constraint) frames NSL as legitimate security restoration with moderate extractiveness targeting political opposition; democratic_enclosure_reading frames NSL as permanent closure of democratic space with high extractiveness targeting all dissent; jurisdictional_capture_reading frames NSL as vehicle for mainland legal system transplantation with institutional-level power transfer. Each reading instantiates a different constraint with different victim/beneficiary sets, different ε values, and different classified types. The three are linked via network.affects_constraints because each reading's enforcement affects the viability of the others — if sovereignty_restoration succeeds in stabilizing opposition suppression, democratic_enclosure and jurisdictional_capture readings become harder to sustain as live political positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
