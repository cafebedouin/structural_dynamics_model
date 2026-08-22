% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto as Westphalian Consent Sovereignty (Sovereignty Reading)
 *   domain: international relations/institutional design/constitutional law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Article 27 veto kernel: the
 *   sovereignty reading, under which the veto is the Westphalian consent
 *   principle — no state can be bound by international law without its
 *   consent — applied to the five states whose enforcement capacity is
 *   genuinely global. On this reading the clause is descriptive of material
 *   reality rather than constitutive of privilege: any institution empowered
 *   to compel great-power action would face the same coordination failure, so
 *   the veto adds nothing to extract. The claim and the metrics are
 *   independent authored facts: the claimed type (mountain) and the near-zero
 *   metric profile are both authored from this reading's seat, without tuning
 *   either to a predicted engine output — where the engine's per-seat
 *   computations diverge from the claim, that divergence is the measurement
 *   the corpus exists to take, and the contest itself lives in the sibling
 *   readings, not in this file. KEY AGENTS (by structural relationship): -
 *   p5_member_states: holders of absolute negative control
 *   (institutional/arbitrage) — their material position persists with or
 *   without the clause - non_p5_member_states: navigating majority
 *   (organized/constrained) — accepted the structure as the price of
 *   great-power participation - un_security_council_elected_members: rotating
 *   participants (moderate/constrained) — hold agenda voice that exists only
 *   because permanent membership is closed - crisis_affected_populations:
 *   absent affected parties (powerless/trapped) — bear the consequences of
 *   blocked action with no seat - comparative_constitutional_scholars:
 *   analytical observer — sees the full structure, alters no votes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.09).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.18).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.09).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto as Westphalian Consent Sovereignty (Sovereignty Reading)").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international relations/institutional design/constitutional law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, '90419fac-afb5-4aed-88a2-56ad47584aa5').
narrative_ontology:cs_kernel_codification('90419fac-afb5-4aed-88a2-56ad47584aa5', fixed_text).
narrative_ontology:cs_authority_grounding('90419fac-afb5-4aed-88a2-56ad47584aa5', lineage).
narrative_ontology:cs_interpretation_layer_present('90419fac-afb5-4aed-88a2-56ad47584aa5').
narrative_ontology:cs_reading_relation('90419fac-afb5-4aed-88a2-56ad47584aa5', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('90419fac-afb5-4aed-88a2-56ad47584aa5', article_27_veto_power__oligopoly_reading, influences).
narrative_ontology:cs_axiom('90419fac-afb5-4aed-88a2-56ad47584aa5', foundational, binding_requires_state_consent).
narrative_ontology:cs_axiom_status(binding_requires_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('90419fac-afb5-4aed-88a2-56ad47584aa5', binding_requires_state_consent, conventional).
narrative_ontology:cs_axiom('90419fac-afb5-4aed-88a2-56ad47584aa5', secondary, authority_tracks_enforcement_capacity).
narrative_ontology:cs_axiom_status(authority_tracks_enforcement_capacity, holdable).
narrative_ontology:cs_axiom_grounding('90419fac-afb5-4aed-88a2-56ad47584aa5', authority_tracks_enforcement_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('90419fac-afb5-4aed-88a2-56ad47584aa5', westphalian_consent_sovereignty).
narrative_ontology:cs_drift_state('90419fac-afb5-4aed-88a2-56ad47584aa5', contemporary_multipolar_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('90419fac-afb5-4aed-88a2-56ad47584aa5', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__sovereignty_reading, non_p5_member_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__sovereignty_reading, un_security_council_elected_members).
narrative_ontology:constraint_victim(article_27_veto_power__sovereignty_reading, non_p5_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__sovereignty_reading, un_security_council_elected_members).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, westphalian_consent_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states holding permanent Security Council seats, each able to block any substantive resolution and, through the Article 108 ratification requirement, any Charter amendment. Each possesses independent global military reach and nuclear arsenals, so their compliance with collective-security decisions has never depended on the clause. They describe the blocking right as recognition of their special responsibilities, cast or withhold it at negligible material cost to themselves, and would occupy the same position with or without the text: leaving the arrangement would not change their capabilities, staying merely formalizes what those capabilities already determine.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, p5_member_states, agenda_setter,
    institutional, generational, arbitrage, global).

% The remaining member states, which accepted the blocking arrangement at founding and by later accession as the price of great-power participation in collective security. They coordinate through the General Assembly and cross-regional groups to press veto-restraint codes and reform proposals that cannot amend the Charter without the five's ratification. They receive predictable signals about which Council actions will command great-power acquiescence, and they carry the standing risk that action they seek will be blocked.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, non_p5_member_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__sovereignty_reading, non_p5_member_states, beneficiary).

% Ten states elected to two-year Council terms. The seats confer agenda voice, drafting influence, and diplomatic prestige that exist only because permanent membership is closed; their leverage ends where a permanent member's dissent begins. When their term expires they revert to ordinary membership, and the value of having held the seat depends on the permanent-member structure remaining as it is.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, un_security_council_elected_members, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__sovereignty_reading, un_security_council_elected_members, payer).

% Civilian populations in territories where Council action — intervention mandates, arms embargoes, accountability referrals — has been blocked by a permanent member's dissent, frequently shielding a patron or ally. They hold no seat in the Council and reach it only through governments that may themselves depend on the shielding patron. When action is blocked they absorb the consequences directly and have no procedural channel of their own.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, crisis_affected_populations, excluded,
    powerless, immediate, trapped, regional).

% Analysts who study the arrangement against other constitutional orders — league systems, confederations, supermajority structures — and publish on whether the blocking right tracks material capacity or manufactures privilege. They take no side in Council decisions; their assessments feed reform debates and diplomatic history without altering any vote.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns the Security Council's formal decision authority with the actual distribution of global enforcement capacity: no collective-security decision binds a great power without that power's acquiescence, keeping the Council's outputs inside the envelope of what will actually be complied with and keeping the great powers inside the institution rather than outside defying it.
% TRANSFER_FUNCTION: Concentrates absolute negative control over collective-security decisions and Charter amendment in five states; in exchange the general membership receives assured great-power participation in, and acquiescence to, authorized action. Little material wealth moves; what moves is decision rights and assurance.
% ABSENT_VOICES: Populations in territories shielded by third-party vetoes have no seat; the defeated Axis powers were excluded from the 1945 design conversation; and later member generations are bound by a power distribution fixed before they existed — pointed, because the reading grounds legitimacy in consent, yet most of the arrangement's current subjects never individually consented to it.
% DISAPPEARANCE_RATIONALE: Material arrangements derive from the underlying distribution of enforcement capacity, not from Article 27: overnight repeal would produce a Council passing resolutions that great powers ignore, eroding the institution's credibility without changing any great power's conduct. The formal-legal layer would shift — Charter amendment unlocked, third-party shielding ended — which is exactly why the sibling readings dispute this verdict, but no state's material position depends on the clause.
% FOUNDING_PROBLEM: After the League of Nations collapsed when defiant great powers exited rather than comply, the 1945 designers sought to keep the great powers inside a collective-security institution; the conceded price was giving each of them an absolute blocking right, on the theory that an institution great powers ignore is worse than one they can slow but not break.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the permanent five: the League of Nations' documented failure sequence (Japanese and Italian withdrawal rather than compliance) attests the founding problem; Dumbarton Oaks and San Francisco negotiating records show small states objecting to the veto and accepting it expressly as the price of great-power participation; diplomatic-history scholarship by non-beneficiary academics confirms the design intent.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_unchanged).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.09, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored near-zero (0.09 at interval end) because the reading's referent is the standing arrangement assessed by its own lights: measured against the counterfactual in which great powers are compelled, nothing is taken from anyone, since compulsion was never available. The series shows a narrow band with a Cold War rise (accumulating third-party shielding vetoes), a post-Cold War dip (cooperative-era great-power acquiescence reduced the shielding burden), and renewed upward drift with returning multipolarity — never approaching extractive thresholds. Suppression is low (0.18) and static: the clause coerces no one directly, and its main foreclosing effect (the Article 108 amendment lockout) suppresses institutional-evolution attempts that, on this reading, could not deliver compulsion anyway; suppression is a raw structural property, unscaled by power or scope, and no suppression_requirement series is authored because the veto has no enforcement machinery whose build-up or decay could be traced — it is self-executing. Accessibility collapse is high (0.88): once nuclear weapons and global-reach enforcement capacity are internalized, alternative institutional designs collapse, because no constitution compels a nuclear great power. Resistance is low (0.12): Uniting-for-Peace resolutions, restraint codes, and transparency initiatives are real but rhetorically contained, never threatening the clause's operation. Theater is low but rising (0.08 to 0.22): the veto performs its stated function throughout, while the widening gap between reform rhetoric and structural stasis adds performative maintenance on top. All series run on one shared nine-point grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from identical structural data. From the p5_member_states seat, the arrangement is recognition: the clause formalizes what their capabilities already determine, their exit is arbitrage-grade (removal changes nothing for them), and effective extraction inverts toward subsidy. From the crisis_affected_populations seat — powerless and trapped — whatever extraction exists is amplified toward the full-target end, because they bear blocked-action costs with no exit and no seat. The elected-member seat sits between: genuine participation value, bounded leverage. The engine computes these per-seat classifications from power, exit, and scope; this story's near-zero base epsilon does not predetermine them, because directionality and scope scaling are the engine's arithmetic, not the author's.
 *
 * DIRECTIONALITY LOGIC:
 *   Consistent with the reading's structural claim, no beneficiaries and no victims are declared: the constraint is held to mirror the material distribution of enforcement capacity rather than to move goods between parties, so the derivation chain falls back to canonical per-power-atom defaults rather than structural derivation. That fallback is acceptable here and no directionality_overrides are authored, because there is no beneficiary/victim declaration for an override to correct. Qualitatively: the permanent five sit slightly off the pure-beneficiary end only in the thin sense that lawful pre-sanctioned dissent lowers the reputational cost of defiance they would incur anyway; the general membership sits near symmetric (ceded blocking rights in exchange for guaranteed participation); crisis-affected populations sit nearest the target end via trapped exit, which is where the engine will find the story's only meaningful per-seat extraction signal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing great-power participation after the League's collapse — is still live, so no mandatrophy is declared: the arrangement has not outlived its function on this reading. The mountain claim guards against the opposite error: reading the veto as a snare would recommend abolition as the fix, when on this reading abolition reproduces the League's failure mode rather than curing anything. The claim is kept falsifiable rather than immune: the natural-law-versus-constructed-privilege question is carried as an explicit omega, and the oligopoly sibling reading holds the extraction-centered account this file deliberately does not. If the founding problem ever dies — great powers no longer needing coaxing to stay inside collective security — the clause would persist as maintained habit, the theater_ratio series is the instrument watching for that transition, and the classification would migrate accordingly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_natural_vs_constructed,
    'Is the veto a genuine structural feature of great-power politics, or a constructed privilege maintained by Charter entrenchment that benefits identifiable agents?',
    'Compare permanent-member defiance costs and patterns inside versus outside institutional contexts: if defiance frequency and consequences are statistically indistinguishable with and without formal veto coverage, the clause is epiphenomenal; if formal legality measurably lowers defiance costs, the clause adds constructed privilege beyond material capacity.',
    'A constructed-privilege finding would collapse the near-zero epsilon, support reclassification toward the oligopoly reading''s territory, and convert the mountain claim into a false-summit detection case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_natural_vs_constructed, empirical, 'Whether the veto is natural law or constructed privilege — the false-summit question for this reading.').

omega_variable(
    third_party_shielding_scope,
    'Does the consent principle extend to third-party shielding — a permanent member blocking action against a client state that itself consents to nothing?',
    'Doctrinal analysis separating self-protection vetoes from client-protection vetoes in the complete voting record, with share estimates for each category across the interval.',
    'If a substantial share of vetoes shield third parties, the constraint exceeds the consent principle it claims to instantiate, and the near-zero epsilon understates extraction borne by the shielded populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_shielding_scope, empirical, 'Whether client-shielding vetoes exceed the consent principle''s scope.').

omega_variable(
    counterfactual_baseline_ambiguity,
    'What is the correct counterfactual for measuring extraction — a world with no Security Council, or a world with a Council lacking the veto?',
    'Analyze cases where draft resolutions blocked by anticipated vetoes, or their General Assembly equivalents, nonetheless generated compliance pressure through legitimacy and reputational mechanisms despite non-enforceability.',
    'If normative pressure sometimes moves great powers, the veto''s suppression of such resolutions is real extraction invisible under the no-institution baseline, and epsilon rises accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_baseline_ambiguity, conceptual, 'Baseline choice determines whether the veto''s blocking function counts as extraction.').

omega_variable(
    accession_consent_defect,
    'Most member states never individually consented to the veto — they acceded to an existing constitutional order. Does consent-based legitimacy survive accession?',
    'Comparative constitutional analysis of accession-versus-original-consent legitimacy, together with a survey of accession-time reservations and declarations lodged by joining states.',
    'If accession consent is judged defective, the sovereignty reading''s own grounding undermines the arrangement''s legitimacy — the mountain claim loses its normative floor even if its descriptive accuracy stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accession_consent_defect, conceptual, 'Whether the consent principle grounds an arrangement most subjects joined rather than consented to.').

omega_variable(
    enforcement_asymmetry_persistence,
    'Is great-power enforcement asymmetry permanent, or historically contingent on the nuclear deterrence regime and current technology?',
    'Strategic-studies analysis of enforcement-capacity convergence scenarios: missile defense maturation, precision-strike diffusion, proliferation cascades, and non-nuclear global-reach capabilities.',
    'If the asymmetry is contingent, the impossibility premise weakens and the veto becomes transitional support for a particular strategic era rather than a structural feature — moving the classification away from mountain toward scaffold-like temporality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_asymmetry_persistence, empirical, 'Whether the physical premise of the inevitability claim persists across technological regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 1946, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1946, article_27_veto_power__sovereignty_reading, theater_ratio, 1946, 0.08).
narrative_ontology:measurement_basis(arti_tr_t1946, observed).
narrative_ontology:measurement(arti_tr_t1955, article_27_veto_power__sovereignty_reading, theater_ratio, 1955, 0.11).
narrative_ontology:measurement_basis(arti_tr_t1955, observed).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__sovereignty_reading, theater_ratio, 1965, 0.13).
narrative_ontology:measurement_basis(arti_tr_t1965, observed).
narrative_ontology:measurement(arti_tr_t1975, article_27_veto_power__sovereignty_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement_basis(arti_tr_t1975, observed).
narrative_ontology:measurement(arti_tr_t1985, article_27_veto_power__sovereignty_reading, theater_ratio, 1985, 0.13).
narrative_ontology:measurement_basis(arti_tr_t1985, observed).
narrative_ontology:measurement(arti_tr_t1995, article_27_veto_power__sovereignty_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement_basis(arti_tr_t1995, observed).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__sovereignty_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement_basis(arti_tr_t2005, observed).
narrative_ontology:measurement(arti_tr_t2015, article_27_veto_power__sovereignty_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(arti_tr_t2015, observed).
narrative_ontology:measurement(arti_tr_t2025, article_27_veto_power__sovereignty_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(arti_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t1946, article_27_veto_power__sovereignty_reading, base_extractiveness, 1946, 0.04).
narrative_ontology:measurement_basis(arti_be_t1946, observed).
narrative_ontology:measurement(arti_be_t1955, article_27_veto_power__sovereignty_reading, base_extractiveness, 1955, 0.05).
narrative_ontology:measurement_basis(arti_be_t1955, observed).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__sovereignty_reading, base_extractiveness, 1965, 0.06).
narrative_ontology:measurement_basis(arti_be_t1965, observed).
narrative_ontology:measurement(arti_be_t1975, article_27_veto_power__sovereignty_reading, base_extractiveness, 1975, 0.07).
narrative_ontology:measurement_basis(arti_be_t1975, observed).
narrative_ontology:measurement(arti_be_t1985, article_27_veto_power__sovereignty_reading, base_extractiveness, 1985, 0.07).
narrative_ontology:measurement_basis(arti_be_t1985, observed).
narrative_ontology:measurement(arti_be_t1995, article_27_veto_power__sovereignty_reading, base_extractiveness, 1995, 0.05).
narrative_ontology:measurement_basis(arti_be_t1995, observed).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__sovereignty_reading, base_extractiveness, 2005, 0.06).
narrative_ontology:measurement_basis(arti_be_t2005, observed).
narrative_ontology:measurement(arti_be_t2015, article_27_veto_power__sovereignty_reading, base_extractiveness, 2015, 0.08).
narrative_ontology:measurement_basis(arti_be_t2015, observed).
narrative_ontology:measurement(arti_be_t2025, article_27_veto_power__sovereignty_reading, base_extractiveness, 2025, 0.09).
narrative_ontology:measurement_basis(arti_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the P5 veto' decomposes into three structurally distinct constraints sharing one kernel. This file (sovereignty_reading) authors the consent-principle instantiation: epsilon near-zero, mountain profile, no beneficiary/victim structure. coordination_reading authors the war-prevention mechanism account; oligopoly_reading authors the entrenchment-and-rents account with substantially positive epsilon. The sovereignty reading is upstream of the oligopoly reading in legitimation: its consent narrative is precisely the resource the entrenchment critique says is exploited, which is why the reading relation is influences rather than mere coexistence. All three files link one another via affects_constraints per the constraint-family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
