% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__gradual_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__gradual_transition_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__gradual_transition_reading
 *   human_readable: Managed Dual-Script Transition Regime (Gradual Reading of the Turkish Graphemic Substrate)
 *   domain: political linguistics / state formation / cultural engineering
 *
 * SUMMARY:
 *   In the actual 1928 reform the Turkish state replaced the Ottoman Arabic
 *   alphabet within months, severing most living readers from the public text
 *   in a single season. This story instantiates the losing alternative — the
 *   gradual_transition_reading of the turkish_graphemic_substrate kernel — as
 *   a clean, epsilon-invariant constraint: a statutorily bounded regime of
 *   dual graphemic validity, five to fifteen years long, in which official
 *   publication, schooling, and administration operate in both alphabets
 *   under a published phase-out calendar terminating in Latin-script
 *   officialdom. The colloquial label 'the Turkish alphabet reform'
 *   decomposes, per the epsilon-invariance principle, into three structurally
 *   distinct claims — permanent Arabic legitimacy, immediate exclusive Latin
 *   legitimacy, and this managed temporary dual validity — each with its own
 *   epsilon, victim set, and temporal profile; the siblings are separate
 *   stories linked through network.affects_constraints. The epsilon referent
 *   here is the dual-script regime itself, assessed by this reading's own
 *   lights: the arrangement is endorsed by its authors, and the authored
 *   metrics describe its real but deliberately bounded costs, not a hostile
 *   outside estimate. Claim and metrics are independent facts: the claimed
 *   type is scaffold because the arrangement's justification is the
 *   transition and its statute carries a terminal date; the metrics describe
 *   what operating it actually costs while it stands.
 *
 * KEY AGENTS:
 *   - transition_authority: agenda-setter (institutional/arbitrage) — writes the calendar and owns the sunset
 *   - pre_transition_ottoman_literates: protected seat (moderate/constrained) — keeps literacy without relearning
 *   - islamic_scholarly_establishment: identity-locked beneficiary (organized/identity_locked) — subsidized obsolescence
 *   - transition_generation_students: primary cost-bearing seat (powerless/trapped) — double burden, dual inheritance
 *   - rural_teaching_corps: delivery-side cost bearer (moderate/constrained)
 *   - state_administrative_apparatus: execution-side cost bearer (institutional/constrained)
 *   - ottoman_script_printers_publishers: concentrated commercial recipient (moderate/constrained)
 *   - modernization_hardliners: excluded flank (powerful/arbitrage)
 *   - comparative_transition_analysts: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, 0.36).
domain_priors:suppression_score(turkish_graphemic_substrate__gradual_transition_reading, 0.22).
domain_priors:theater_ratio(turkish_graphemic_substrate__gradual_transition_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__gradual_transition_reading, scaffold).
narrative_ontology:human_readable(turkish_graphemic_substrate__gradual_transition_reading, "Managed Dual-Script Transition Regime (Gradual Reading of the Turkish Graphemic Substrate)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__gradual_transition_reading, "political linguistics / state formation / cultural engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__gradual_transition_reading).
narrative_ontology:has_sunset_clause(turkish_graphemic_substrate__gradual_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__gradual_transition_reading, '60295260-500b-4bf6-8849-7d2348e79603').
narrative_ontology:cs_kernel_codification('60295260-500b-4bf6-8849-7d2348e79603', formalized).
narrative_ontology:cs_authority_grounding('60295260-500b-4bf6-8849-7d2348e79603', lineage).
narrative_ontology:cs_interpretation_layer_present('60295260-500b-4bf6-8849-7d2348e79603').
narrative_ontology:cs_reading_relation('60295260-500b-4bf6-8849-7d2348e79603', turkish_graphemic_substrate__ottoman_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('60295260-500b-4bf6-8849-7d2348e79603', turkish_graphemic_substrate__secular_nationalist_reading, influences).
narrative_ontology:cs_axiom('60295260-500b-4bf6-8849-7d2348e79603', foundational, intergenerational_textual_continuity_is_binding).
narrative_ontology:cs_axiom_status(intergenerational_textual_continuity_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('60295260-500b-4bf6-8849-7d2348e79603', intergenerational_textual_continuity_is_binding, deontological).
narrative_ontology:cs_axiom('60295260-500b-4bf6-8849-7d2348e79603', secondary, dual_validity_is_temporary_by_design).
narrative_ontology:cs_axiom_status(dual_validity_is_temporary_by_design, holdable).
narrative_ontology:cs_axiom_grounding('60295260-500b-4bf6-8849-7d2348e79603', dual_validity_is_temporary_by_design, instrumental).
narrative_ontology:cs_reference_frame('60295260-500b-4bf6-8849-7d2348e79603', bounded_dual_validity_transition).
narrative_ontology:cs_drift_state('60295260-500b-4bf6-8849-7d2348e79603', comparative_transition_record_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60295260-500b-4bf6-8849-7d2348e79603', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, pre_transition_ottoman_literates).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, islamic_scholarly_establishment).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, ottoman_script_printers_publishers).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, transition_generation_students).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, rural_teaching_corps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, transition_generation_students).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, ottoman_script_printers_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and administers the dual-validity statute: publishes the phase-out calendar, funds dual-script teacher training, sets minimum dual-edition quotas for official gazettes and school primers, and owns the terminal date after which Latin alone carries official business. It can shorten or extend the calendar by legislative act; no other seat holds that pen.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, transition_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Adults educated under the Ottoman system — clerks, merchants, clergy, literati — whose reading and writing remain legally valid for the window's duration. They need not relearn anything to keep receiving official notices, contracts, and correspondence, though each scheduled phase shrinks the space where their script appears. Learning the Latin alphabet late in life is possible for a few and out of reach for most.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, pre_transition_ottoman_literates, beneficiary,
    moderate, biographical, constrained, national).

% Keeps preaching, catechism, and scholarly correspondence legible to ordinary believers while the Arabic-letter tradition retains official standing. Its self-understanding is bound to the script of revelation and the manuscript chain that transmits it; it petitions for extensions and resists compression of the calendar. The published terminal date reads, from inside this seat, as a scheduled dispossession with a grace period.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, islamic_scholarly_establishment, beneficiary,
    organized, generational, identity_locked, continental).

% The compulsory-school cohort drilled in both alphabets at once: doubled penmanship hours, doubled spelling lists, examinations in two orthographies. In exchange they alone grow up able to read both the Ottoman manuscript inheritance and the emerging Latin-print corpus. Attendance is compulsory; no household can opt its children out.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, transition_generation_students, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, transition_generation_students, beneficiary).

% Village instructors, many freshly certified themselves, must acquire a second script and deliver a doubled curriculum with scarce primers and crowded classrooms. The burden arrives as assigned duties; declining it means leaving the profession rather than escaping the workload.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, rural_teaching_corps, payer,
    moderate, biographical, constrained, national).

% Line ministries, provincial governorships, and municipal offices run parallel registries, forms, letterheads, and gazette editions in both scripts for the calendar's duration. Duplication is ordered from the center and absorbed locally without matching budget lines; refusing it is career-ending insubordination.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, state_administrative_apparatus, payer,
    institutional, generational, constrained, national).

% Hold guaranteed demand for dual editions, official print contracts, and primer runs for as long as the window lasts, while simultaneously retooling foundries and compositors for Latin typefaces ahead of the terminal date. Their franchise is lucrative precisely because the statute schedules its expiry.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, ottoman_script_printers_publishers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, ottoman_script_printers_publishers, payer).

% The rapid-rupture faction — militant secularists, younger cadres, parts of the officer corps — argues the window squanders the revolutionary moment, doubles public expense, and gives reaction time to regroup. Excluded from the gradualist settlement, they campaign through newspapers, lecture halls, and barracks to collapse the calendar.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, modernization_hardliners, excluded,
    powerful, generational, arbitrage, national).

% Later scholars of script reform who assess the window's design against the record of other alphabet switches — Uzbekistan's stalled decades-long dualism, Kazakhstan's dated phases, Turkmenistan's reversal. They collect nothing and bear nothing; their seat is retrospective and comparative.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, comparative_transition_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__gradual_transition_reading, ottoman_script_printers_publishers).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__gradual_transition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps written communication legible across the script boundary while the substrate changes: official notices reach Ottoman-literate recipients, family correspondence crosses the generational divide, archives and religious texts stay consultable by the laity, and the Latin corpus grows without severing the old one — a migration corridor between two information standards operated under a published schedule.
% TRANSFER_FUNCTION: Moves implementation resources — doubled print runs, parallel registry labor, doubled classroom hours, retooled foundries — from the treasury, the administrative apparatus, and the school-age cohort's time, and delivers from them continued textual access for the Ottoman-literate generations, guaranteed demand for the Arabic-side print trade, and an unbroken intergenerational channel during the switch.
% ABSENT_VOICES: Two voices are missing from the gradualist settlement. The rapid-rupture modernizers — powerful, organized, and ultimately victorious in actual history — are excluded from this reading's coalition and would collapse the calendar given the chance. The Anatolian peasantry, overwhelmingly illiterate in either script, is argued about rather than consulted: the cheapest path to functional literacy for them might be neither bridge nor rupture but mass primary schooling in one script, a priority no seat at the table represents.
% DISAPPEARANCE_RATIONALE: If the dual-validity statute vanished mid-window, the rearrangement is immediate: official notices, contracts, and textbooks revert to Latin alone, the Ottoman-literate plurality loses the public text overnight — the rupture the window exists to prevent, arriving anyway — dual print contracts and dual curricula collapse, the printers' franchise dies ahead of schedule, and the clerical constituency loses laity reach in a single season. Mid-window, a great deal of daily arrangement depends on the bridge holding.
% FOUNDING_PROBLEM: The 1928 reformers faced a genuine dilemma: modernization and European alignment demanded the Latin alphabet, but instant replacement made a literate generation functionally illiterate overnight, sealed the archival and religious corpus behind an unreadable script, and cut grandparents off from grandchildren's schoolbooks. The gradual reading was built to solve one problem: how to change a civilization's writing substrate without amputating its literate past.
% FOUNDING_PROBLEM_CORROBORATION: No seat inside the winning rapid-transition settlement corroborated the gradual diagnosis — the rupture faction treated the generational break as a benefit, not a cost, and the historical record contains no beneficiary-party attestation of this reading's problem statement. Corroboration is retrospective and external: provincial education inspectorate reports documenting the post-1928 adult illiteracy spike; the documented plight of Ottoman-trained officials unable to read their own ministry files; minority and religious-community petitions of the period; and the later comparative record (Uzbekistan, Kazakhstan, Mongolia) in which successor states adopted phased or dual arrangements explicitly citing rupture costs. That the corroboration is entirely outside and after the fact is itself signal: the reading lost its own founding debate.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__gradual_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__gradual_transition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__gradual_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__gradual_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__gradual_transition_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).
:- end_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and hump-shaped rather than monotone: it climbs as the dual burdens stack (two curricula, parallel registries, doubled print quotas all running at once around the window's middle), then unwinds on schedule as cohorts finish dual instruction and Arabic-side obligations lapse. Suppression is markedly lower than the rupture path's because the regime persuades and schedules rather than bans — it must hold two flanks in line (publishers tempted to drop the Arabic side early, clerical constituencies tempted to make dualism permanent) but it criminalizes no one's literacy. Theater is low through the working middle of the window — dual editions and dual classrooms are real labor — and rises toward the sunset as compliance turns ceremonial in offices where only the Latin file is ever consulted. Accessibility_collapse sits low: the alternatives (faster rupture, open-ended dualism) stayed arguable throughout, which is precisely what the hardliner flank exploited. Resistance is moderate and bidirectional. All three series share one time grid (t=0,3,6,9,12,15) so no metric is sampled against another's end-state; every point carries basis 'projected' because the window is a counterfactual instantiation — the values are calibrated model judgments from the comparative transition record, not observations of a realized Turkish window. Receipt surface: the window's material flows concentrate measurably in the print trade — guaranteed dual-edition demand and state contracts — so gain_flow names the printers rather than 'diffuse'; the scholarly establishment's receipts are status and reach, not the extracted surplus. Fixing cost: removability is the design — the sunset is self-executing statute, no seat holds a veto over the terminal date, and closing the window is an act of legislative will rather than a fight against entrenched beneficiaries; 'cheap' is judged for the authority at the terminal date, with mid-window closure politically dearer, which the window_duration omega tracks.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the transition_authority's chair the window is logistics: calendars, quotas, budgets, a terminus it owns. From the pre_transition_ottoman_literates' chair it is a reprieve with a countdown — every phase-out notice is simultaneously protection and eviction papers. From the students' chair it is double homework now against an inheritance no sibling arrangement grants. From the hardliners' chair it is squandered momentum; from the scholarly establishment's chair, subsidized obsolescence. The engine derives these divergences from power, exit, and directional position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directional positions: Ottoman literates (protected access, no relearning burden), the scholarly establishment (continued laity reach), printers (guaranteed demand). Declared victims map to high positions: the student cohort (compulsory double burden), the administrative apparatus (unbudgeted duplication), the rural teaching corps (delivery-side overload). Two seats need overrides because the derivation reads only declared role and exit. The scholarly establishment derives near-full-beneficiary from its beneficiary declaration and identity_locked exit, but the derivation cannot see that its benefit is sunset-scheduled — the statute dates its subsidy's death, so its true position sits nearer the middle (override organized -> 0.30). The student cohort derives near-full-target from its victim declaration, but it is the only seat that uniquely gains from the arrangement — dual-corpus access neither sibling reading provides — so its net position is meaningfully less targeted than the raw victim declaration implies (override powerless -> 0.60).
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim guards against two mislabels. Read as pure coordination, the window's real asymmetric burdens — the cohort's time, the treasury's doubling, the rural corps' overload — disappear into a benign aggregate. Read as pure extraction, the self-liquidating design vanishes: unlike a snare, this arrangement's statute schedules its own dissolution, and no seat's persistence depends on keeping it. The live mandatrophy risk runs forward, not backward. The comparative record says managed windows slip: Uzbekistan's transition has been 'temporary' for three decades; Turkmenistan reversed outright. If this window misses its sunset, the scaffold decays toward a hybrid (dualism serving printer and clerical constituencies who now profit from permanence) or toward inert ceremonial dualism that everyone funds and no one uses. The theater_ratio series rising toward the sunset is the early symptom of exactly that decay; the sunset_compliance_uncertainty omega names the mechanism; the cheap fixing_cost is the structural safeguard — the authority can still close the window by act of will, which is what distinguishes a scaffold from an entrenched regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the managed dual-script window the correct instantiation of the turkish_graphemic_substrate kernel, or do the sibling readings — permanent Arabic legitimacy, immediate exclusive Latin legitimacy — capture the structure the kernel actually commits to?',
    'Adjudicate on the contested variable: whether substrate legitimacy is a permanent identity attribute (the siblings'' premise) or a schedulable administrative state (this reading''s premise), tested against how later republics structure their own script transitions and against the internal logic of the 1928 statute''s own justificatory language.',
    'Sibling adoption swaps the victim set (rupture cohort versus heritage-loss cohort), adds or removes the sunset, and shifts epsilon — the rupture path concentrates harm instantly at higher magnitude; the permanence path diffuses it indefinitely; this reading bounds and schedules it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the graphemic-substrate kernel this constraint correctly instantiates.').

omega_variable(
    window_duration_sufficiency,
    'Is five to fifteen years long enough to complete intergenerational knowledge transfer yet short enough to prevent the dual regime from entrenching?',
    'Demographic literacy-cohort modeling of transfer completion rates, benchmarked against comparative durations — Uzbekistan''s open-ended dualism as the too-long failure mode, Kazakhstan''s dated phases as the design template.',
    'Too short a window reproduces the rupture at lower intensity and vindicates the hardliners; too long a window converts the scaffold into permanent dualism with entrenched printer and clerical constituencies, moving the classification toward hybrid extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(window_duration_sufficiency, empirical, 'Whether the declared window length threads the transfer-versus-entrenchment needle.').

omega_variable(
    sunset_compliance_uncertainty,
    'Does the window actually close on schedule, or does the Arabic-script constituency convert the temporary arrangement into permanent dualism?',
    'Statutory review milestones with published compliance audits; comparative record of slipped transitions (Uzbekistan''s three-decade ''temporary'' dualism, Turkmenistan''s reversal) as the base rate for managed-window slippage.',
    'A missed sunset is the mandatrophy event: the scaffold decays toward a hybrid serving concentrated print and clerical interests, or toward inert ceremonial dualism — the theater_ratio trajectory in the measurements is the leading indicator.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_compliance_uncertainty, empirical, 'Whether the sunset clause executes or the window slips into permanence.').

omega_variable(
    implementation_cost_incidence,
    'Who actually bears the doubled implementation costs — the treasury, the rural schools, the retooling printers, or the student cohort''s study time?',
    'Fiscal incidence analysis of dual-track budgets plus time-use reconstruction for dual-curriculum cohorts, checked against the comparative record of transition financing.',
    'Incidence concentrating on powerless seats raises effective extraction above the authored 0.36 and shades the arrangement toward hybrid extraction; diffuse incidence across capable seats supports the transitional-support reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_cost_incidence, empirical, 'The real incidence of the window''s doubled costs across seats.').

omega_variable(
    avoided_rupture_magnitude,
    'How large is the generational rupture the window avoids, relative to the actual rapid transition''s documented costs?',
    'Counterfactual calibration from rupture-cost evidence: the post-1928 adult illiteracy spike, archival access loss, the stranded Ottoman-trained officialdom, and analogous abrupt-switch cases in later republics.',
    'A small avoided rupture means the window''s extra costs buy little and the secular_nationalist reading dominates on efficiency; a large avoided rupture means the window''s premium is justified and this reading dominates on welfare grounds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(avoided_rupture_magnitude, empirical, 'The size of the rupture cost the gradual path purchases relief from.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__gradual_transition_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(turk_tr_t0, projected).
narrative_ontology:measurement(turk_tr_t3, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 3, 0.13).
narrative_ontology:measurement_basis(turk_tr_t3, projected).
narrative_ontology:measurement(turk_tr_t6, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(turk_tr_t6, projected).
narrative_ontology:measurement(turk_tr_t9, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement_basis(turk_tr_t9, projected).
narrative_ontology:measurement(turk_tr_t12, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(turk_tr_t12, projected).
narrative_ontology:measurement(turk_tr_t15, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(turk_tr_t15, projected).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(turk_be_t0, projected).
narrative_ontology:measurement(turk_be_t3, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 3, 0.37).
narrative_ontology:measurement_basis(turk_be_t3, projected).
narrative_ontology:measurement(turk_be_t6, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement_basis(turk_be_t6, projected).
narrative_ontology:measurement(turk_be_t9, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 9, 0.46).
narrative_ontology:measurement_basis(turk_be_t9, projected).
narrative_ontology:measurement(turk_be_t12, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(turk_be_t12, projected).
narrative_ontology:measurement(turk_be_t15, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement_basis(turk_be_t15, projected).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(turk_su_t0, projected).
narrative_ontology:measurement(turk_su_t3, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 3, 0.33).
narrative_ontology:measurement_basis(turk_su_t3, projected).
narrative_ontology:measurement(turk_su_t6, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 6, 0.39).
narrative_ontology:measurement_basis(turk_su_t6, projected).
narrative_ontology:measurement(turk_su_t9, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 9, 0.4).
narrative_ontology:measurement_basis(turk_su_t9, projected).
narrative_ontology:measurement(turk_su_t12, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement_basis(turk_su_t12, projected).
narrative_ontology:measurement(turk_su_t15, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement_basis(turk_su_t15, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__gradual_transition_reading, information_standard).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__secular_nationalist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Turkish alphabet reform.' The label conflates three structurally distinct claims: (1) ottoman_continuity_reading — Arabic script as the permanent legitimate substrate, a persistent identity constraint with no sunset; (2) secular_nationalist_reading — Latin as sole legitimate substrate with rupture as constitutive, the historically realized rapid-replacement constraint; (3) this story — managed temporary dual validity with a declared Latin terminus, a bounded transitional-support constraint. Each has its own epsilon, victim set, and temporal profile; measuring one with another's observable violates epsilon-invariance. This reading sits downstream of both siblings: it presupposes the modernization terminus the secular-nationalist reading supplies while borrowing the continuity valuation the Ottoman-continuity reading supplies, and its window materially reshapes both siblings' operating environments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, organized, 0.3).
constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
