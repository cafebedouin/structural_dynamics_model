% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country, Two Systems — Autonomy-Primacy Reading (Treaty-Guaranteed Autonomy)
 *   domain: constitutional/political/international
 *
 * SUMMARY:
 *   This story instantiates the autonomy_primacy_reading of the
 *   one_country_two_systems_framework kernel: the claim that the Joint
 *   Declaration and Basic Law guarantee Hong Kong substantive autonomy with
 *   meaningful checks on mainland interference, that civil liberties and
 *   judicial independence are treaty-guaranteed and internationally
 *   enforceable, and that mainland intervention beyond the enumerated fields
 *   is treaty violation. Per the kernel-reading epsilon rule, the epsilon
 *   referent is the standing arrangement under contest — the framework as
 *   actually operated through the national-security-law era — assessed by
 *   this reading's own lights, which is why epsilon is high: this reading
 *   measures the current operation as large-scale extraction of the
 *   guaranteed liberties. The claim/metric gap is deliberate and independent:
 *   the reading CLAIMS the framework should be a protected coordination
 *   settlement, while the authored METRICS describe what the standing
 *   arrangement actually does as this reading sees it. Sibling readings
 *   (sovereignty_primacy_reading, balanced_coexistence_reading) are separate
 *   constraints in separate files, linked through the network block; they are
 *   not averaged into this one.
 *
 * KEY AGENTS:
 *   - prc_central_authorities: agenda-setter (institutional/arbitrage) — drafts, interprets, amends, and enforces the governing texts; collects sovereign control and the framework's international-facing benefits
 *   - hk_pro_democracy_civil_society: primary target (powerless/identity_locked) — bears prosecution, party dissolution, and exile; staying is constitutive
 *   - hk_independent_journalists: primary target (powerless/trapped) — bears press closure and arrest exposure; their professional function is what enforcement removes
 *   - hongkong_residents: diffuse target-and-beneficiary (moderate/constrained) — keep the separate economic system, pay the narrowed liberties, exit only at high personal cost
 *   - hk_judiciary: empowered-and-pressured intermediary (institutional/constrained) — holds review powers the framework grants while administering the security docket under political ceiling
 *   - hk_business_establishment and multinational_gateway_firms: coordination beneficiaries (powerful and organized/arbitrage) — collect the distinctiveness premium; loyalty is portfolio-level
 *   - international_treaty_parties: analytical observer (institutional/analytical) — monitor, report, sanction; hold no enforcement mechanism inside the arrangement
 *   - taiwan_public_and_government: excluded stakeholder (institutional/analytical) — highest stakes in the framework's credibility as a unification template, no seat anywhere in it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.78).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.84).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country, Two Systems — Autonomy-Primacy Reading (Treaty-Guaranteed Autonomy)").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional/political/international").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:has_sunset_clause(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, 'f340e4d4-79e5-407f-b16f-a667c96be0bc').
narrative_ontology:cs_kernel_codification('f340e4d4-79e5-407f-b16f-a667c96be0bc', fixed_text).
narrative_ontology:cs_authority_grounding('f340e4d4-79e5-407f-b16f-a667c96be0bc', lineage).
narrative_ontology:cs_interpretation_layer_present('f340e4d4-79e5-407f-b16f-a667c96be0bc').
narrative_ontology:cs_reading_relation('f340e4d4-79e5-407f-b16f-a667c96be0bc', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f340e4d4-79e5-407f-b16f-a667c96be0bc', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('f340e4d4-79e5-407f-b16f-a667c96be0bc', foundational, treaty_autonomy_internationally_enforceable).
narrative_ontology:cs_axiom_status(treaty_autonomy_internationally_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('f340e4d4-79e5-407f-b16f-a667c96be0bc', treaty_autonomy_internationally_enforceable, conventional).
narrative_ontology:cs_axiom('f340e4d4-79e5-407f-b16f-a667c96be0bc', foundational, civil_liberties_not_revocable_by_sovereign_discretion).
narrative_ontology:cs_axiom_status(civil_liberties_not_revocable_by_sovereign_discretion, holdable).
narrative_ontology:cs_axiom_grounding('f340e4d4-79e5-407f-b16f-a667c96be0bc', civil_liberties_not_revocable_by_sovereign_discretion, deontological).
narrative_ontology:cs_reference_frame('f340e4d4-79e5-407f-b16f-a667c96be0bc', joint_declaration_treaty_autonomy_baseline).
narrative_ontology:cs_drift_state('f340e4d4-79e5-407f-b16f-a667c96be0bc', post_national_security_law_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('f340e4d4-79e5-407f-b16f-a667c96be0bc', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authorities).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hk_business_establishment).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, multinational_gateway_firms).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hk_pro_democracy_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hk_independent_journalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hongkong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hk_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hongkong_residents).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hk_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the Basic Law, retain its interpretation and amendment powers through the Standing Committee, station the garrison, and since 2020 impose and administer national security legislation directly over the territory. Collect restored sovereign control over Hong Kong affairs while retaining the framework's international-facing benefits — the financial-center standing, the treaty record, the gateway function. Their exit from the arrangement's constraints is unilateral: they can reinterpret or amend the texts that bind every other participant, and they have done so repeatedly.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Live under the arrangement's daily operation: they keep the separate currency, common-law courts, and open commercial order the framework preserves, and they carried the prosperity the transition delivered. Since 2020 they also carry the costs: speech, assembly, and press narrowed by security offenses, an electorate re-filtered through a vetting committee, school curricula rewritten, and a documented emigration wave. Leaving is possible through foreign visa schemes but means surrendering property, careers, language community, and often family ties, so exit is real but expensive.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hongkong_residents, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hongkong_residents, beneficiary).

% Organized the primaries, the mass marches, and the independent press that tested the autonomy promise; since 2020 its parties have dissolved, its newspaper closed, its leaders been prosecuted or gone into exile. For those who remain, staying is the point — their civic identity is constituted by the city and its promised freedoms, so the available exit reads as abandonment rather than relief. Those already exiled continue the work from abroad and cannot return without exposure to arrest, which binds them to the cause at distance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hk_pro_democracy_civil_society, payer,
    powerless, biographical, identity_locked, regional).

% Ran the last mass-circulation independent paper until its closure under the security law; remaining editors and reporters face arrest exposure, licensing pressure, and a collapsed advertising base. Their professional function — testing official accounts — is precisely what the current enforcement machinery removes, and relocating abroad dissolves the beat, the sources, and the audience that made the work possible.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hk_independent_journalists, payer,
    powerless, biographical, trapped, regional).

% Holds the review powers the framework grants — testing administrative acts and ordinances against the Basic Law — and draws its international standing from that mandate. Since 2020 it also administers the security docket under designated-judge rules without juries, and overseas judges have declined appointment or resigned from the bench as the political ceiling on rulings became visible. Individual judges can resign; the institution cannot move its jurisdiction, and its legitimacy rides on decisions it increasingly does not control.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hk_judiciary, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hk_judiciary, payer).

% Collects the framework's economic dividend: separate customs territory, capital convertibility, common-law contracts enforceable in a credible court, and privileged access to both Western and mainland markets. Capital is mobile and the comparable alternative jurisdiction is a short flight away, so their attachment to the arrangement is portfolio-level rather than civic — they will re-domicile if the distinctiveness premium disappears, and they price enforcement headlines into that calculation continuously.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hk_business_establishment, beneficiary,
    powerful, biographical, arbitrage, global).

% Use the territory as the treaty-backed interface between their home jurisdictions and mainland markets — regional headquarters, listing venue, arbitration seat. They wrote the framework's guarantees into site-selection models and re-price them downward as enforcement actions accumulate; their exit cost is a relocation budget, not a life, and several have already shifted regional functions to rival hubs while retaining the listing presence.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, multinational_gateway_firms, beneficiary,
    organized, immediate, arbitrage, global).

% Signed or witnessed the Joint Declaration, registered it at the United Nations, and monitor compliance through six-monthly reports, annual certification requirements, and human-rights review cycles. They impose targeted sanctions, export controls, and visa measures when their assessments fail, but hold no enforcement mechanism inside the arrangement itself; their seat is evaluative — they record the gap between promise and operation and cannot close it.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_parties, observer,
    institutional, generational, analytical, global).

% Watch the arrangement as the advertised template for cross-strait unification; every stage of its operation recalibrates their assessment of any future offer and their security posture accordingly. They were never party to any negotiation and hold no seat in the framework, yet its credibility is a direct input to their survival calculations — the constituency with arguably the most at stake in the framework's honesty and the least voice in any of its forums.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, taiwan_public_and_government, excluded,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authorities).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the interface between two legal and economic systems under one sovereign: preserves a common-law jurisdiction, a separate customs territory, a convertible currency, and an open capital account inside a socialist one-party state, solving the 1997 transfer problem without war or economic rupture.
% TRANSFER_FUNCTION: Moves political authority over security, candidacy, and — increasingly — speech from Hong Kong institutions to PRC central bodies; moves civil liberties from residents into state discretion; historically moved stability guarantees, prosperity, and international market access outward to residents and firms.
% ABSENT_VOICES: Hong Kong residents were never consulted on the Joint Declaration; the elected pro-democracy legislators who objected to the Basic Law's final text had no vote on it and boycotted the drafting process's closing stages; Taiwan, the constituency with the largest stake in the framework's credibility as a unification template, holds no seat anywhere in it. Unanimity in the framework's founding documents arose from a room that contained none of these parties.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, the legal status of 7.5 million people becomes unresolved, the separate customs territory and currency arrangements dissolve, markets reprice the territory's entire financial function within days, pending treaties and trade preferences lapse, and Taiwan's unification calculus shifts immediately — the region's diplomatic and economic architecture is arranged around this settlement and would rearrange around its absence.
% FOUNDING_PROBLEM: Resolving the sovereignty transfer of a treaty-acquired capitalist enclave without war, preserving its economic function and its residents' way of life while asserting sovereign control — Deng-era formulation: let the territory's system remain while sovereignty returns.
% FOUNDING_PROBLEM_CORROBORATION: No fully neutral arbiter exists, but attestation from outside the benefiting parties is available: the UK Foreign Office six-monthly reports document erosion of the guaranteed freedoms; UN human-rights treaty-body reviews and the ICCPR reporting cycle record the narrowing; US State Department certifications under the Hong Kong Policy Act and successive EU conclusions independently assess the autonomy guarantees as substantially diminished. Central authorities attest the founding problem remains live and is being solved (stability, prosperity, integration); the treaty-monitoring record from outside the beneficiary set corroborates instead that the autonomy-specific founding problem has been abandoned in operation while the sovereignty-transfer problem was genuinely solved.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.78 at interval end) is high because the standing arrangement, by this reading's lights, transfers security control, candidacy control, and speech from Hong Kong institutions and residents to central discretion while retaining the framework's economic branding. Suppression (0.84) is higher still because persistence since 2020 depends on dedicated enforcement machinery — a security police unit, designated judges, no-jury trials, candidate vetting, media closure — not on participant assent. Theater ratio (0.60) reflects the widening decoupling of framework performance (anniversary ceremonies, vetted elections presented as representation, 'patriots administering' rhetoric) from substantive function, while real functions (commercial courts, customs territory, currency board) continue quietly. Accessibility collapse (0.62) is substantial but incomplete: reliance on courts to check security decisions and on elections to produce opposition has collapsed, but physical exit via emigration schemes remains open at cost. Resistance (0.52) captures dismantled organized domestic resistance alongside continuing diaspora advocacy, international reporting cycles, and latent discontent. The measurement series run on one shared seven-point grid (t=0,13,26,33,36,39,41 mapping 1984-2025) so every tracked metric is authored at every examined point. The trajectory is a ratchet with one step-change at t=36 (imposition of the security law), not a cycle; the 2003 withdrawal of the local security bill after mass protest is the single reversal, absorbed into the smoothed series. Suppression_requirement is tracked deliberately: the story's central dynamic is the construction and hardening of enforcement capacity, which the scalar base_properties.suppression alone cannot date.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the agenda-setter seat the arrangement is a sovereign's own settlement, revisable at will — extraction is invisible because the interpreter of the constraint is its principal beneficiary. From the business seats the same structure reads as stable coordination worth a location premium, with arbitrage-grade exit keeping their effective burden low. From the identity-locked civil-society seat and the trapped journalist seat the identical structure operates as enforced extraction with no internal remedy. The judiciary occupies the hinge: institutionally empowered by the reading's premises, practically constrained by the security docket's political ceiling — its computed position should sit between beneficiary damping and target amplification, which the structural data supports only weakly since it appears in neither the beneficiary nor victim arrays; its directionality falls back to the power-atom default, and this residual ambiguity is acknowledged rather than papered over with an override.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive damping: prc_central_authorities (agenda-setter, arbitrage exit — they author and reinterpret the texts) derive near-full-beneficiary directionality; hk_business_establishment and multinational_gateway_firms (arbitrage exit, mobile capital) sit nearest the beneficiary pole. Victim declarations drive amplification: hk_pro_democracy_civil_society (identity_locked — exit reads as abandoning the constitutive commitment) and hk_independent_journalists (trapped — the profession does not survive relocation) sit near the full-target pole, with identity lock pushing them past what structural barriers alone would imply. hongkong_residents hold dual position (payer with secondary beneficiary) and constrained exit, landing mid-range with net target lean. International treaty parties are observers (analytical exit) and take no extraction. No directionality overrides are used: the role, exit, and power data already differentiate the seats, and the one genuinely ambiguous seat (hk_judiciary) is flagged in commentary rather than forced with a power-atom-keyed override that would also capture the other institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transferring sovereignty over a capitalist enclave without war while preserving its economic function — was solved: the 1997 handover proceeded, markets held, and the transition-management mandate completed within roughly a decade. Mandatrophy is therefore declared resolved: what persists is not the original mandate but a contested governance settlement whose justification is now disputed between readings. This classification prevents two opposite mislabels. Reading the standing arrangement as pure extraction ignores the coordination functions that demonstrably persist — a common-law jurisdiction enforcing commercial contracts, a separate customs territory, a convertible currency, a listing venue both capital pools still use — which is why the claim is tangled_rope rather than snare despite the post-2020 extraction profile. Reading it as intact coordination ignores the asymmetric extraction the same structure now delivers through actively enforced machinery, which is why the metrics are authored high and the temporal series shows the drift. The declared 2047 horizon keeps the scaffold question live: if the sunset is reaffirmed as genuine, the transitional justification strengthens; if discarded, the last restraint on accumulation falls (see omega horizon_2047_settlement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel one_country_two_systems_framework — the autonomy_primacy_reading. What would the sibling readings change structurally?',
    'Compare compiled stories: sovereignty_primacy_reading re-authors mainland intervention as lawful sovereign revision (epsilon drops toward coordination-cost floor, victim set empties); balanced_coexistence_reading splits epsilon across negotiated domains and relocates boundary disputes from courts to political accommodation.',
    'The disagreement is located in one structural element: whether the treaty creates enforceable limits on sovereign action. Resolving it selects which sibling''s victim set and enforcement structure governs classification; this story''s high epsilon is valid only inside this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: this story instantiates one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    epsilon_unit_decomposition,
    'Is the standing arrangement correctly measured as one framework-level constraint, or should it decompose into an economic-distinctiveness component (customs territory, currency, common-law commerce) and a security-political component (security legislation, candidacy control, speech)?',
    'Test whether the two components'' epsilon values diverge irreducibly across observables: commercial-rule-of-law indicators versus civil-liberty indicators. Divergence mandates separate stories linked by network.affects_constraints.',
    'If decomposed, this story''s epsilon applies to the security-political component; the economic component likely authors materially lower epsilon and a different beneficiary structure. Framework-level aggregation would then overstate extraction on commercial seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_unit_decomposition, conceptual, 'Whether the framework is one epsilon-bearing constraint or a family of two with divergent extraction profiles.').

omega_variable(
    treaty_enforceability_mechanism,
    'Does the reading''s core premise — internationally enforceable autonomy — have any operative enforcement channel, or is international accountability purely declaratory?',
    'Comparative efficacy audit of measures taken 2020-2025: targeted sanctions, export controls, visa schemes, UN treaty-body reviews, and six-monthly reporting. Measure whether any measure altered central-authority behavior rather than only recording violation.',
    'If no channel is operative, the enforceability axiom is honorific and the constraint''s suppression is unsanctioned externally, raising effective extraction on trapped seats; if channels bite at the margin, part of the measured suppression is priced-in friction rather than unchecked extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_enforceability_mechanism, empirical, 'Operative versus declaratory status of the international enforceability premise.').

omega_variable(
    resident_exit_composition,
    'Is continued residence by targeted civil-society members driven by identity lock (staying as a constitutive act) or by structural barriers (custody, passport restrictions, care duties, asset illiquidity)?',
    'Post-exit trajectory comparison: emigrants who leave voluntarily versus those barred from leaving; re-offer acceptance rates when barriers lift; stated-intention surveys weighted against revealed behavior.',
    'If identity lock dominates, exit-option modulation amplifies effective extraction beyond what structural barriers alone predict, and suppression persists after physical exit (exiles remain bound); if structural barriers dominate, easing them converts trapped seats into mobile ones and lowers measured extraction without any change in the arrangement itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resident_exit_composition, empirical, 'Composition of the exit constraint on targeted residents: internalized versus structural.').

omega_variable(
    horizon_2047_settlement,
    'Will the framework''s terminal date (2047) be honored as a genuine sunset, renegotiated, or discarded in favor of indefinite integration?',
    'Observe central-authority statements and legislative preparation as 2047 approaches; track whether transition planning treats the horizon as binding, extendable, or void.',
    'A reaffirmed sunset gives the arrangement a transitional justification and pulls classification toward scaffold-flavored readings; discarding the horizon entrenches the arrangement as permanent and removes the last restraint on accumulation, pulling the trajectory further toward pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(horizon_2047_settlement, preference, 'Status of the framework''s declared terminal horizon and its classificatory consequence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 0, 41).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octs_autonomy_primacy_tr_t0, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(octs_autonomy_primacy_tr_t13, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 13, 0.1).
narrative_ontology:measurement(octs_autonomy_primacy_tr_t26, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 26, 0.22).
narrative_ontology:measurement(octs_autonomy_primacy_tr_t33, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 33, 0.35).
narrative_ontology:measurement(octs_autonomy_primacy_tr_t36, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 36, 0.48).
narrative_ontology:measurement(octs_autonomy_primacy_tr_t39, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 39, 0.55).
narrative_ontology:measurement(octs_autonomy_primacy_tr_t41, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 41, 0.6).

% Extraction over time
narrative_ontology:measurement(octs_autonomy_primacy_be_t0, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(octs_autonomy_primacy_be_t13, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 13, 0.18).
narrative_ontology:measurement(octs_autonomy_primacy_be_t26, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 26, 0.32).
narrative_ontology:measurement(octs_autonomy_primacy_be_t33, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 33, 0.45).
narrative_ontology:measurement(octs_autonomy_primacy_be_t36, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 36, 0.7).
narrative_ontology:measurement(octs_autonomy_primacy_be_t39, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 39, 0.76).
narrative_ontology:measurement(octs_autonomy_primacy_be_t41, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 41, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(octs_autonomy_primacy_su_t0, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(octs_autonomy_primacy_su_t13, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 13, 0.15).
narrative_ontology:measurement(octs_autonomy_primacy_su_t26, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 26, 0.28).
narrative_ontology:measurement(octs_autonomy_primacy_su_t33, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 33, 0.45).
narrative_ontology:measurement(octs_autonomy_primacy_su_t36, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 36, 0.75).
narrative_ontology:measurement(octs_autonomy_primacy_su_t39, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 39, 0.82).
narrative_ontology:measurement(octs_autonomy_primacy_su_t41, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 41, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'One Country, Two Systems' covers three structurally distinct claims about who decides the limits of Hong Kong autonomy. This file carries the autonomy-primacy claim (treaty-created enforceable limits; intervention is violation; high epsilon over the standing arrangement). sovereignty_primacy_reading carries the revocable-delegation claim (intervention is lawful sovereign revision; epsilon near coordination floor; empty victim set). balanced_coexistence_reading carries the negotiated-boundary claim (epsilon split across domains; disputes relocated from courts to politics). The upstream/downstream structure runs from the treaty-text baseline shared by all three: whichever reading prevails rewrites the others' enforcement conditions, so each file links the other two via affects_constraints. Decomposition follows the epsilon-invariance principle: one reading, one constraint, one stable epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
