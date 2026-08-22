% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocity Covenant: Royal Obligations Bound to Cosmic Balance
 *   domain: ancient history/political philosophy/religious studies
 *
 * SUMMARY:
 *   Ancient Egyptian political theology held that the king's rule and the
 *   cosmos stood or fell together: so long as the king upheld Ma'at — truth,
 *   justice, right order — the Nile flooded, the sun crossed the sky, and the
 *   valley prospered. This story instantiates the RECIPROCITY reading of that
 *   kernel: Ma'at is a mutual covenant, not a one-way emanation. The king
 *   owes his subjects justice, stability, and proper distribution of grain
 *   and protection; they owe him surplus and labor; and a king who defaults
 *   forfeits the claim on both. The arrangement solves a real coordination
 *   problem (Nile-scale water management, storage, adjudication) while
 *   channeling a substantial surplus upward to palace and temple — hence the
 *   tangled_rope claim. The colloquial label 'Ma'at' covers several
 *   structurally distinct claims; per the epsilon-invariance rule this file
 *   authors only the reciprocity reading, with the sibling readings
 *   (divine_mandate_reading, distributed_maintenance_reading) linked as
 *   separate constraints whose epsilon values and victim sets differ. KEY
 *   AGENTS (by structural relationship): - pharaoh_of_egypt: Agenda-setting
 *   sovereign bound by the covenant (institutional/identity_locked) —
 *   administers enforcement, captures the surplus, bears the performance
 *   obligations - egyptian_commoners: Dual-positioned base of the covenant
 *   (powerless/constrained) — receive justice, stability, and famine relief;
 *   pay grain tax and corvee - corvee_laborers: Primary extraction targets
 *   (powerless/trapped) — drafted for construction, quarrying, mining,
 *   transport - grain_tax_households: Annual surplus payers
 *   (powerless/constrained) — assessed and collected from by scribal teams -
 *   temple_priesthood: Institutional beneficiary
 *   (institutional/identity_locked) — endowed estates, cult performance,
 *   durable records - royal_scribal_officialdom: Administrative beneficiary
 *   (organized/constrained) — staffs assessment, collection, granaries,
 *   courts - provincial_nomarchs: Leveraged intermediaries (powerful/mobile)
 *   — collect and remit; can and did withhold support when the center
 *   defaulted - tribute_subject_peoples: Excluded outsiders
 *   (powerless/trapped) — fund the system from beyond the covenant boundary -
 *   wisdom_tradition_scribes: Analytical observers (organized/analytical) —
 *   state the standard and grade kings against it
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.58).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.36).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocity Covenant: Royal Obligations Bound to Cosmic Balance").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient history/political philosophy/religious studies").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, 'e0f76bf3-ad0d-400b-80a2-50dd27ab7340').
narrative_ontology:cs_kernel_codification('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', distributed).
narrative_ontology:cs_authority_grounding('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', practice).
narrative_ontology:cs_interpretation_layer_present('e0f76bf3-ad0d-400b-80a2-50dd27ab7340').
narrative_ontology:cs_reading_relation('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', foundational, pharaoh_bound_by_reciprocal_obligation).
narrative_ontology:cs_axiom_status(pharaoh_bound_by_reciprocal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', pharaoh_bound_by_reciprocal_obligation, deontological).
narrative_ontology:cs_axiom('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', secondary, failed_obligation_licenses_withdrawal_of_support).
narrative_ontology:cs_axiom_status(failed_obligation_licenses_withdrawal_of_support, holdable).
narrative_ontology:cs_axiom_grounding('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', failed_obligation_licenses_withdrawal_of_support, instrumental).
narrative_ontology:cs_reference_frame('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', covenantal_reciprocal_sovereignty).
narrative_ontology:cs_drift_state('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', intermediate_period_collapses, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e0f76bf3-ad0d-400b-80a2-50dd27ab7340', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, egyptian_commoners).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, royal_scribal_officialdom).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, corvee_laborers).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, grain_tax_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, grain_tax_households).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, provincial_nomarchs).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, pharaoh_of_egypt).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, egyptian_commoners).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, provincial_nomarchs).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, maat_cosmic_balance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits at the apex: commissions canals, granaries, courts, and temples; orders the collection of grain tax and the drafting of labor levies; performs the daily rites the ideology holds load-bearing for the flood and the sun's course. The same ideology that authorizes his takings binds him to deliver justice, stability, and provision in return, and the memory of collapsed predecessors presses the obligation on him. Leaving the role is not something he can act on — the office and the cosmic function are fused in his titulary and his afterlife prospects.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh_of_egypt, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, pharaoh_of_egypt, payer).

% Farm the flood plain, deliver a harvest share to palace and temple stores, and supply labor levies in the inundation slack season. In return they receive adjudication of disputes, dike and canal maintenance, famine relief from royal granaries in bad years, and protection. Flight to the frontier or attachment to a powerful household is possible but costly, and leaves kin and fields behind.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, egyptian_commoners, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, egyptian_commoners, payer).

% Are drafted for construction, quarrying, mining expeditions, and transport, usually by village quota. During a levy they work under gang discipline far from home and desertion is punishable. Service is periodic rather than perpetual, but its timing and duration are set entirely from above.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, corvee_laborers, payer,
    powerless, biographical, trapped, regional).

% Owe a measured share of the harvest to the state. Assessment and collection run through scribes and local headmen; shortfall brings beatings or seizure, as collection papyri record. Bad years do not reliably cancel the assessment, though remission is occasionally proclaimed.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, grain_tax_households, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, grain_tax_households, beneficiary).

% Hold endowed estates worked by tenants and dedicated laborers, and perform the daily cult whose performance the ideology treats as necessary for cosmic order. Their income, status, and identity are constituted by the arrangement, and they are its most durable record-keepers.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, temple_priesthood, beneficiary,
    institutional, generational, identity_locked, national).

% Staff the census, assessment, collection, granary accounting, and courts. They advance by service and are paid from the very flows they administer. They enforce the covenant's demands downward and petition its protections upward; their careers depend on the system continuing.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, royal_scribal_officialdom, beneficiary,
    organized, generational, constrained, national).

% Govern named districts: raise the levies, keep a share, remit the rest, maintain local cults and courts. When the center delivers on its obligations they remit; when it visibly fails they retain revenue, date by their own regnal years, and field their own men — as the First Intermediate Period record shows.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, provincial_nomarchs, beneficiary,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, provincial_nomarchs, payer).

% Communities beyond the covenant boundary — Nubia and the Levantine coast — deliver tribute and captives under garrison pressure. The reciprocal protections advertised inside Egypt are not extended to them; they fund the arrangement without standing inside it.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, tribute_subject_peoples, excluded,
    powerless, biographical, trapped, continental).

% Copy and compose the instruction and complaint literature that states the standard and grades kings and officials against it. They serve palace and temple yet produce the sharpest surviving criticism of both; their seat is evaluative, not administrative.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, wisdom_tradition_scribes, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__reciprocity_reading, pharaoh_of_egypt).
narrative_ontology:fixing_cost_class(maat_order_principle__reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates decisions about Nile-wide water management, granary storage, dispute adjudication, and defense in one accountable center, so flood-cycle agriculture can support cities, craft specialization, and monument building without each district arming itself against its neighbors.
% TRANSFER_FUNCTION: Moves grain, livestock, linen, and labor from farming households up to palace and temple stores, and moves protection, adjudication, irrigation work, and famine relief back down; a further stream of tribute and captives moves inward from beyond the frontier.
% ABSENT_VOICES: Tribute-subject peoples would object that they carry the system's costs with none of its protections; landless laborers and women hold thin formal voice inside the covenant — complaint literature speaks of the widow, but no widows speak in it. They are absent outside the covenant boundary the ideology itself draws and below the property line its texts assume.
% DISAPPEARANCE_RATIONALE: If the reciprocal covenant vanished overnight, the legitimacy case for remitting grain and answering levies collapses with it: districts would retain revenue, local strongmen would field their own men, and the canal-granary-court package would fragment into competing principalities. The First Intermediate Period is the recorded rehearsal of exactly this rearrangement.
% FOUNDING_PROBLEM: Early Dynastic Egypt needed a way to make one center's claim on surplus and labor acceptable across the whole valley: why obey, feed, and build for a distant court? The reciprocity formulation answered with a bargain — the king maintains justice, stability, and provision, and in exchange the valley sustains him.
% FOUNDING_PROBLEM_CORROBORATION: Attested from inside the scribal class but not from outside the beneficiary set: the Instruction for Merikare and the Tale of the Eloquent Peasant state the reciprocal standard while castigating officials, and the Admonitions of Ipuwer describes the world when the bargain fails. No source attests the commoner side in commoner words — literacy ran through palace and temple schools — so the covenant's popular half survives only as elite reportage, and the corpus contains no fully external corroborator.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58: the covenant moved a large annual surplus (harvest share plus inundation-season labor) upward, but the reciprocity norm both promised returns and supplied a standing argument against unlimited taking — a ceiling honored mainly in crises. Suppression is 0.36: the arrangement rarely needed heavy coercion because it delivered visible returns, and it licensed complaint (the Eloquent Peasant's successful appeal) rather than forbidding it; enforcement capacity nonetheless swung widely over the interval (see the suppression_requirement series). Theater is 0.41 at interval end: daily cult and royal iconography performed real legitimation work throughout, but by the late New Kingdom ritual continued at full splendor while wages fell into arrears and tombs were robbed — performance outrunning delivery. Accessibility_collapse is 0.42: alternatives (provincial autonomy, flight, negotiated withholding) remained exercisable, as the First Intermediate Period proved. Resistance is 0.45: labor flight, the Deir el-Medina strikes, complaint literature, and nomarch defection.
 *   
 *   CYCLICAL PATTERN: the series traces roughly one and a half cycles of the characteristic arc — accumulation (Old Kingdom corvee peak) -> crisis and collapse (First Intermediate Period, when nomarchs withheld and the center's extraction fell with its capacity) -> reform and restoration (Middle Kingdom) -> renewed accumulation (New Kingdom empire) -> late-New-Kingdom decay (arrears, strikes, enforcement attrition). The oscillation is partly the mechanism itself: extraction ratchets upward until crisis forcibly resets it, an intermittent-reinforcement dynamic at civilizational scale — the covenant's protections arrive episodically (famine relief, remissions) while its demands arrive annually. Base_properties scalars are measured at T20, the late-accumulation/decay phase of the second cycle. Interval units are centuries: T0 approximates 3000 BCE (early Dynastic unification), T20 approximates 1050 BCE (late New Kingdom). Suppression_requirement is tracked because enforcement-capacity change is the dynamic this story traces: it built to Old Kingdom mobilization peaks, decayed with the center in the First Intermediate Period, was rebuilt under the Middle Kingdom, and attrited again in the late New Kingdom when strikes went unanswered.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the corvee_laborer seat — powerless, trapped, high directionality — the arrangement presents as enforced extraction with episodic compensation. From the pharaoh seat — identity-locked into an office fused with the cosmic function — the same structure presents as a binding covenant he administers and is judged by. From the provincial_nomarch seat — powerful, with mobile exit — it presents as a renegotiable contract: remit when the center delivers, withhold when it does not, and the historical record shows withholding exercised. From the temple_priesthood seat — identity-locked beneficiary — it presents as sacred duty whose performance is itself the point. The engine derives these divergences from the power, exit, and role data; the divergence, not any single seat's verdict, is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: egyptian_commoners, temple_priesthood, and royal_scribal_officialdom sit toward the beneficiary end (damped effective extraction); corvee_laborers and grain_tax_households sit toward the target end (amplified). The pharaoh appears in neither array: he simultaneously captures the surplus (pulling him toward the beneficiary end) and bears the covenant's performance obligations (pushing toward the target end), a net position the canonical fallback cannot express. Directionality overrides were considered and deliberately NOT authored: the override surface keys on power_atom alone, and every candidate atom here contains seats with opposed positions — powerless spans corvee_laborers (near-full targets) and protected commoners; institutional spans the capturing pharaoh and the endowed priesthood. An atom-level override would misstate one side of each pair, so the dual positions are documented here and in the stakeholder situations and left to the engine. Scaling note: suppression is authored as a raw structural property and is not scaled by anything; only extractiveness is scaled, by directionality and spatial scope — the covenant's national scope modestly amplifies effective extraction on target seats because verifying royal performance valley-wide is harder than verifying a household's payment.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves visible. Reading the arrangement as pure snare (ideology as cover for extraction) erases the genuine coordination delivered — canal networks, granaries that rode out lean years, courts a peasant could appeal to, and the fact that the system's own texts armed its critics. Reading it as pure rope (the palace's framing) erases the corvee dead, the seized households, and the tribute peoples outside the covenant. The reciprocity reading specifically guards against mandatrophy error in the obsolescence direction: the founding problem — making one center's claim on surplus and labor acceptable valley-wide — remains live for as long as the arrangement stands, so this is not a piton maintained by inertia; it persisted because it delivered, and it fell, twice, when it stopped. No sunset clause is authored because the arrangement carried none: its justification was steady-state covenant, not transition, and its eventual transformation came by conquest and priestly usurpation, not by design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (reciprocity_reading) of the maat_order_principle kernel; the sibling readings divine_mandate_reading and distributed_maintenance_reading instantiate different constraints from the same kernel — what structurally changes under each sibling?',
    'Compile and classify the sibling stories and compare victim sets, extraction ceilings, and enforcement requirements across the family.',
    'Under the divine mandate reading the king is definitionally unable to violate Ma''at: the accountability ceiling disappears, resistance loses legitimacy, and effective extraction is uncapped. Under the distributed maintenance reading obligation diffuses across all stations: no seat can be specifically held, the royal ceiling weakens, and enforcement becomes everyone''s and no one''s. Either sibling would reclassify this arrangement''s accountability structure wholesale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame routing of the kernel contest: which reading governs determines whether royal accountability exists at all.').

omega_variable(
    ceiling_enforcement_basis,
    'Is the reciprocity ceiling on royal extraction enforced by a working institutional mechanism, or only retroactively by crisis?',
    'Compare reigns with documented obligation-failure that did not end in collapse (late-Ramesside wage arrears, for instance): if extraction persisted unchecked for generations after violation, the ceiling is crisis-enforced only.',
    'If the ceiling is crisis-only, the arrangement alternates between tangled_rope operation and snare episodes, and the effective long-run extraction rate exceeds what the normative texts promise; if an institutional mechanism existed (petition, oracle, judicial review), the ceiling is real and the rope component is stronger than the metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceiling_enforcement_basis, empirical, 'Whether the moderate extraction ceiling attributed to the reciprocity norm had teeth between crises.').

omega_variable(
    commoner_voice_ventriloquism,
    'Does the reciprocity norm reflect actual popular moral expectation among the taxed and drafted, or elite scribal projection — every surviving source was written by the crown- and temple-trained literate class?',
    'Non-textual evidence: settlement abandonment patterns, labor-flight destinations, village-level material culture, and the geography of desertion compared against the textual claims of willing exchange.',
    'If the covenant''s popular half is projection, the reciprocity framing is cover and the extraction assessment shifts toward snare territory; if it is genuine, the coordination function is robust and the tangled_rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commoner_voice_ventriloquism, empirical, 'Source-bias ambiguity: the covenant''s beneficiary-side voice reaches us only through elite ventriloquism.').

omega_variable(
    cycle_driver_internal_vs_nile,
    'Is the observed accumulation-collapse-restoration cycle driven by internal extraction-ratchet dynamics or by exogenous Nile and climate shocks?',
    'Paleoclimate proxies — Nile flood reconstruction, Lake Moeris levels, Eastern Desert rainfall records — correlated phase-by-phase against the extraction and enforcement series.',
    'If the ratchet is internal, the cycle is itself part of the extraction mechanism (episodic delivery, continuous demand) and the trajectories are endogenous to the constraint; if exogenous, the arrangement is closer to a rope buffeted by environment, and its failures indict resilience rather than structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cycle_driver_internal_vs_nile, empirical, 'Driver ambiguity behind the cyclical measurements: endogenous ratchet versus environmental forcing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(maat_tr_t4, maat_order_principle__reciprocity_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(maat_tr_t8, maat_order_principle__reciprocity_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(maat_tr_t12, maat_order_principle__reciprocity_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(maat_tr_t16, maat_order_principle__reciprocity_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__reciprocity_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(maat_be_t4, maat_order_principle__reciprocity_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(maat_be_t8, maat_order_principle__reciprocity_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(maat_be_t12, maat_order_principle__reciprocity_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(maat_be_t16, maat_order_principle__reciprocity_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__reciprocity_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(maat_su_t4, maat_order_principle__reciprocity_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(maat_su_t8, maat_order_principle__reciprocity_reading, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(maat_su_t12, maat_order_principle__reciprocity_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(maat_su_t16, maat_order_principle__reciprocity_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__reciprocity_reading, suppression_requirement, 20, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, resource_allocation).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Ma'at'. One label covered three structurally distinct claims about who is bound and how accountability works; forcing them into one story would make epsilon observer-relative — the divine mandate reading yields a near-zero accountability ceiling with no legitimate resistance, the distributed maintenance reading diffuses obligation until no seat can be specifically held, and the reciprocity reading yields a conditional ceiling with licensed complaint. Each reading is authored separately with its own epsilon, beneficiaries, and victims. This reciprocity story links to both siblings: a king who can default makes infallibility claims and universal station-duty claims harder to sustain unchallenged, so this reading exerts structural pressure on the others' operating environment without resolving the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
