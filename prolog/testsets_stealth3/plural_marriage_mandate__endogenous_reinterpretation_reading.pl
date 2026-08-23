% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto Regime as Legitimate Prophetic Reinterpretation (Endogenous Reading)
 *   domain: religious/political-theology
 *
 * SUMMARY:
 *   On 25 September 1890, President Wilford Woodruff issued the Manifesto
 *   suspending new plural marriages. This story instantiates the
 *   faithful-institutional reading of that event: the suspension was genuine
 *   revelation, temporally suspending (not retracting) the doctrine,
 *   undertaken to preserve the church's salvific mission — temple ordinances,
 *   sealing authority, and missionary reach — under existential legal threat.
 *   The standing arrangement under contest, and the referent of every metric
 *   here, is the post-Manifesto regime: the suspension itself plus the
 *   enforcement machinery that matured after the 1904 Second Manifesto. Its
 *   beneficiaries are the institution and the general membership; its costs
 *   fall on the minority who maintained the original reading and were
 *   excommunicated for it. The claim and the metrics are independent authored
 *   facts: the claimed type is rope because that is what this reading's own
 *   lights assert — coordination around a new prophetic directive — while the
 *   metrics describe the regime's actual operation, including the asymmetric
 *   costs borne by dissenters. Where the engine's per-seat computation
 *   diverges from the rope claim, that divergence is the measurement this
 *   story exists to take.
 *
 * KEY AGENTS:
 *   - first_presidency_and_twelve: Agenda-setting seat (institutional / identity_locked) — issues and enforces the directive; its warrant and the directive share one revelatory channel
 *   - lds_church_institution: Primary beneficiary (institutional / identity_locked) — collects survival, temple operation, the statehood path, and missionary continuity
 *   - rank_and_file_latter_day_saints: Secondary beneficiary (organized / constrained) — keeps ordinances and community under the revised directive at diffuse cost
 *   - post_manifesto_plural_marriage_practitioners: Primary target (powerless / identity_locked) — bears excommunication for continuing the original practice
 *   - fundamentalist_priesthood_councils: Secondary target (moderate / identity_locked) — organizes the excluded remnant around the original reading
 *   - united_states_federal_establishment: Incidental collector (institutional / arbitrage) — receives legal compliance and the Utah settlement without administering the internal arrangement
 *   - historians_of_the_period: Analytical observer (analytical / analytical) — attests the legal threat and the enforcement sequence from outside confessional commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.45).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.65).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto Regime as Legitimate Prophetic Reinterpretation (Endogenous Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious/political-theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, 'c656acc0-6695-453f-94bc-16568aabaee1').
narrative_ontology:cs_kernel_codification('c656acc0-6695-453f-94bc-16568aabaee1', fixed_text).
narrative_ontology:cs_authority_grounding('c656acc0-6695-453f-94bc-16568aabaee1', lineage).
narrative_ontology:cs_interpretation_layer_present('c656acc0-6695-453f-94bc-16568aabaee1').
narrative_ontology:cs_reading_relation('c656acc0-6695-453f-94bc-16568aabaee1', plural_marriage_mandate__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('c656acc0-6695-453f-94bc-16568aabaee1', plural_marriage_mandate__institutional_pragmatism_reading, forecloses).
narrative_ontology:cs_axiom('c656acc0-6695-453f-94bc-16568aabaee1', foundational, living_prophet_revelation_supersedes_prior_command).
narrative_ontology:cs_axiom_status(living_prophet_revelation_supersedes_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('c656acc0-6695-453f-94bc-16568aabaee1', living_prophet_revelation_supersedes_prior_command, theological).
narrative_ontology:cs_axiom('c656acc0-6695-453f-94bc-16568aabaee1', foundational, salvific_mission_preservation_justifies_temporal_suspension).
narrative_ontology:cs_axiom_status(salvific_mission_preservation_justifies_temporal_suspension, holdable).
narrative_ontology:cs_axiom_grounding('c656acc0-6695-453f-94bc-16568aabaee1', salvific_mission_preservation_justifies_temporal_suspension, instrumental).
narrative_ontology:cs_reference_frame('c656acc0-6695-453f-94bc-16568aabaee1', living_oracle_doctrinal_continuity).
narrative_ontology:cs_drift_state('c656acc0-6695-453f-94bc-16568aabaee1', contemporary_post_schism_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c656acc0-6695-453f-94bc-16568aabaee1', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, rank_and_file_latter_day_saints).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, post_manifesto_plural_marriage_practitioners).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_priesthood_councils).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, united_states_federal_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wilford Woodruff signs the 1890 Manifesto suspending new plural marriages; his successors administer it through apostolic hearings, disciplinary councils, and the 1904 Second Manifesto. The office's warrant and the directive share one revelatory channel: the same authority that announced the original mandate announces its suspension, so abandoning the framework of living prophetic direction would dissolve the ground on which the office itself stands.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, first_presidency_and_twelve, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Retains corporate existence under threat of dissolution under the Edmunds-Tucker Act, keeps temples operating and sealing ordinances flowing, regains the path to Utah statehood, and sustains worldwide missionary work. The suspension purchases continuity the institution could not otherwise secure; the doctrine of plural marriage remains canonized while the practice is withdrawn.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_institution, beneficiary,
    institutional, generational, identity_locked, global).

% Keep membership, temple access, and congregational life under the revised directive, adopting monogamous family norms. They bear diffuse costs: reputational stigma, revision of family expectations, and the quiet strain of members whose parents or relatives lived the prior practice. Leaving means forfeiting the entire covenant community and the eternal-family framework that organizes their lives.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, rank_and_file_latter_day_saints, beneficiary,
    organized, generational, constrained, global).

% Enter or perform new plural marriages after 1890, in Mexico colony settlements, on the high seas, or clandestinely at home, believing the principle eternally required for highest exaltation. They face excommunication, loss of temple access, and severance from family and congregation. Their theology offers no path that preserves both salvation-as-they-understand-it and the practice: remaining loyal to the principle costs them the community; retaining the community costs them the principle.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, post_manifesto_plural_marriage_practitioners, payer,
    powerless, civilizational, identity_locked, regional).

% Organize after exclusion around claims of continuous priesthood authority independent of Salt Lake City, maintaining that the original mandate was never validly rescinded. They absorb excommunication, legal marginalization, and geographic concentration in isolated communities, and they define their collective identity as the remnant faithful to what the mainline abandoned.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_priesthood_councils, payer,
    moderate, civilizational, identity_locked, regional).

% Prosecutes the anti-polygamy campaign: confiscates church property under the Edmunds-Tucker Act, imprisons practitioners, disenfranchises members, and withholds statehood. It collects compliance with federal law, civic order in the territory, and a political settlement, but it neither administers nor participates in the church's internal arrangement; its leverage is environmental, exercised from outside.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, united_states_federal_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Examine congressional debate, court records, apostolic hearing transcripts, and private diaries from outside confessional commitment. They can document the severity of the legal threat, the decade of continued post-Manifesto sealings, and the enforcement sequence after 1904, but possess no instrument for adjudicating whether the signing president's reported revelatory experience was what the tradition says it was.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, historians_of_the_period, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Latter-day Saint community onto a single authoritative directive — suspend new plural marriages — so that temple ordinances, corporate existence, and missionary work continue under one interpretive authority, instead of fragmenting between members who would continue the practice and members who would comply with federal law.
% TRANSFER_FUNCTION: Moves the cost of doctrinal adaptation from the institution and its general membership onto the minority who held the prior reading: practitioners and performers of post-Manifesto plural marriages forfeit membership, temple access, and communal standing, while survival goods — legal existence, temple operation, statehood, missionary reach — flow to the institution and membership broadly.
% ABSENT_VOICES: Ordinary members' individual consciences had no formal channel: assent was aggregated at general conference rather than solicited person by person. Women in existing plural families carried the sharpest practical ambiguities — legal status of children, household economics, inheritance — with no seat in the deliberation. The dissenters were physically present but heard only through disciplinary proceedings whose outcome presupposed the answer.
% DISAPPEARANCE_RATIONALE: Without the Manifesto and its enforcement, the church faced continued property seizure, temple closure, and prosecution of its leadership; the membership would have split between open continuation and quiet compliance decades earlier and along different lines, the Utah political settlement and statehood path would have taken another shape, and the fundamentalist movements that defined themselves against the Manifesto would never have formed as they did.
% FOUNDING_PROBLEM: Reconcile a command the community understood as eternally binding — plural marriage as required for highest exaltation, canonized in Doctrine & Covenants 132 — with an existential legal assault (the Edmunds-Tucker Act, property confiscation, imprisoned leadership) that threatened to end temple ordinances, the rites the community held essential to salvation, altogether.
% FOUNDING_PROBLEM_CORROBORATION: The problem's reality is corroborated from outside the benefiting parties: federal court records, congressional debate on the Edmunds-Tucker Act, and non-Mormon press of 1887-1890 document the confiscation and dissolution threat, and secular historians of the period corroborate the sequence. The divine authorization of the resolution is attested only within the benefiting institution — no outside party corroborates the revelatory claim itself, and the dissenting fundamentalist parties confirm the threat while explicitly denying the revelation.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.45: asymmetric but bounded. Most governed members net-benefit from the regime (the church survived, temples stayed open, statehood arrived), while severe costs concentrate on a small dissenting minority — rising from 0.30 at issuance to a peak of 0.50 as post-1904 enforcement attached full costs to practitioners, then settling at 0.45 once the schism externalized the dissenters. Suppression is 0.65: the regime's persistence depended on active enforcement machinery — apostolic hearings, the resignations of Elders Taylor and Cowley, excommunication of performers and entrants — which the tradition frames as covenant discipline and which is, structurally, coercive maintenance of the directive. The suppression series traces enforcement-capacity buildup (the Second Manifesto ratchet, 1904-1914) followed by partial normalization decay as discipline became routine. Theater is 0.20: the coordination function is real and load-bearing, with modest ceremonial residue in commemorative retelling; the elevated early values (0.38 in 1890) reflect the declaration-versus-practice gap of the 1890-1904 window, closing sharply once enforcement aligned practice with proclamation. Accessibility collapse is 0.50: exits existed — schism, colonization, defiant continuation — but each carried identity-level cost, so alternatives narrowed without vanishing. Resistance is 0.60: a decade of continued secret marriages, apostolic reluctance, and durable fundamentalist counter-organizations. All three series run on one shared grid (1890, 1896, 1902, 1908, 1914, 1921, 1930) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute a harsher type than the rope claim. From the practitioner's position, the same structure that reads as prophetic coordination from the presidency seat reads as expulsion from the channel of salvation: the directive cost them membership, temple access, and the eternal-family framework under their own theology. The beneficiary seats compute rope or softer — genuine collective problem solved at tolerable overhead. The agenda-setter seat experiences the enforcement as fidelity, not coercion. The engine derives this divergence from the structural asymmetry: identity_locked dissenters and an identity_locked agenda-setter stand on opposite sides of the same directive, and power differences (powerless practitioners versus institutional presidency) amplify the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The institution and the rank-and-file are declared beneficiaries, driving their directionalities toward the subsidized end; the practitioners and fundamentalist councils are declared victims with identity_locked exit, placing them near the full-target end — trapped or identity-locked targets sit nearer full extraction than mobile ones, and these dissenters had no exit that preserved both their salvation framework and their community. The federal establishment is deliberately kept OUTSIDE the beneficiaries array: it collected compliance incidentally and administers nothing internal to the arrangement, so listing it would distort the derivation. No directionality override is authored: an override is keyed to a power atom, and the federal seat shares the 'institutional' atom with the church, so any override would misserve one of the two; the structural declarations already produce the correct relationships. Historians carry the analytical seat and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim performs real protective work here: it keeps a genuine survival-coordination arrangement from being misread as pure extraction — the regime solved a real collective problem (existential legal threat to the salvific mission) at moderate coercive overhead, and most participants were net beneficiaries. Conversely, the victim declarations keep the reading's coordination framing from erasing the dissenters' costs: the engine's per-seat computation surfaces the tangled-rope-shaped asymmetry that the insider claim smooths over. The founding problem remains live (the question of how prophetic authority adapts binding command under constraint recurs with every successor administration), and the founding-problem-status x disappearance-verdict pair (live x world_rearranges) produces no zombie flag: this is not a mandate outliving its function but a mandate consciously revised while its function continues. mandatrophy_resolved is therefore not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_disagreement,
    'This story is one reading of kernel plural_marriage_mandate; do the sibling readings (exogenous_override_reading, institutional_pragmatism_reading) merely re-describe the same arrangement, or do they instantiate structurally distinct constraints with different epsilon and victim sets?',
    'Compare the three stories'' beneficiary/victim structures and epsilon values side by side; if the causal-authorial locus (divine initiative versus federal compulsion versus institutional strategy) changes who pays and how much, they are distinct constraints joined by network edges, not one constraint with a measurement parameter.',
    'If distinct, cross-reading epsilon comparison is invalid and each reading''s classification stands alone; if identical, the kernel collapses to a single constraint and the three-way dispute is purely interpretive rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus_disagreement, conceptual, 'Whether sibling readings of the plural-marriage kernel are distinct constraints or one constraint under different labels.').

omega_variable(
    revelatory_claim_outside_accessibility,
    'Can the genuineness of the revelatory experience behind the 1890 Manifesto be assessed from any seat outside the tradition that reports it?',
    'No external instrument accesses another party''s revelatory experience; resolution would require the tradition''s own epistemology (spiritual witness) to be granted evidential standing by the assessing seat — a framing choice, not a measurement.',
    'If inaccessible, this reading''s rope classification rests on a warrant only its own seat can verify, and the engine''s per-seat computation will diverge maximally between insider and outsider seats; if grantable, the classification inherits whatever strength the warrant is granted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelatory_claim_outside_accessibility, conceptual, 'Epistemic accessibility of the revelation claim to non-member assessment.').

omega_variable(
    temporal_suspension_indefiniteness,
    'The reading frames the suspension as temporal; does a century of non-resumption convert ''temporal'' into permanent replacement, and does the reading''s reference frame accommodate that conversion?',
    'Track authoritative statements on future resumption across successive administrations; if each generation reaffirms suspension without a load-bearing resumption expectation, the frame absorbs permanence; if resumption expectation stays doctrinally operative, drift accumulates against the frame.',
    'If the frame absorbs permanence, the constraint settles as long-lived coordination; if not, the widening gap between ''temporary'' framing and indefinite practice drives practice_drift toward eventual reframing pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_suspension_indefiniteness, empirical, 'Whether the ''temporal suspension'' framing survives indefinite non-resumption.').

omega_variable(
    enforcement_onset_date_ambiguity,
    'Did authoritative tolerance of post-Manifesto plural marriages extend until the 1904 Second Manifesto, or were the 1890-1904 sealings unauthorized deviations that the leadership failed to police?',
    'Cross-examine the 1904-1911 apostolic hearing transcripts, Woodruff''s 1891 petition testimony, and sealing records from the Mexico and Canada colonies against contemporaneous private correspondence.',
    'If tolerated, victim costs attach late (post-1904) and early-period extraction is lower than the measured series suggests; if deviant, enforcement failure rather than tolerance explains the gap, and the early suppression series understates suppressive intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_onset_date_ambiguity, empirical, 'Dating the onset of binding enforcement against post-Manifesto plural marriage.').

omega_variable(
    dissenter_exit_lock_composition,
    'Is the dissenters'' persistence in the original reading held in place by structural barriers (excommunication risk, family ties, geographic concentration) or by internalized identity fusion (conviction that plural marriage is required for exaltation)?',
    'Post-exclusion trajectory of individuals who left both the mainline church and the practice: if adherence to the principle persists after all structural penalties lapse, the lock is substantially internalized.',
    'If internalized, the dissenting seat''s effective suppression exceeds the structural measure — the constraint''s costs travel with the dissenter after exit; if structural, removing enforcement would release the seat without residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissenter_exit_lock_composition, empirical, 'Structural versus internalized composition of the dissenters'' identity lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1890, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.38).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1896, 0.34).
narrative_ontology:measurement(plur_tr_t1902, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1902, 0.28).
narrative_ontology:measurement(plur_tr_t1908, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1908, 0.16).
narrative_ontology:measurement(plur_tr_t1914, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1914, 0.14).
narrative_ontology:measurement(plur_tr_t1921, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1921, 0.17).
narrative_ontology:measurement(plur_tr_t1930, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1930, 0.2).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.3).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1896, 0.33).
narrative_ontology:measurement(plur_be_t1902, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1902, 0.36).
narrative_ontology:measurement(plur_be_t1908, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1908, 0.48).
narrative_ontology:measurement(plur_be_t1914, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1914, 0.5).
narrative_ontology:measurement(plur_be_t1921, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1921, 0.47).
narrative_ontology:measurement(plur_be_t1930, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1930, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.4).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1896, 0.42).
narrative_ontology:measurement(plur_su_t1902, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1902, 0.5).
narrative_ontology:measurement(plur_su_t1908, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1908, 0.68).
narrative_ontology:measurement(plur_su_t1914, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1914, 0.72).
narrative_ontology:measurement(plur_su_t1921, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1921, 0.68).
narrative_ontology:measurement(plur_su_t1930, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1930, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the 1890 Manifesto' covers three structurally distinct claims about one event, decomposed per the epsilon-invariance principle. This story (endogenous_reinterpretation_reading) authors epsilon 0.45 for the post-Manifesto regime read as legitimate prophetic coordination, with the institution and membership as beneficiaries and post-Manifesto practitioners plus fundamentalist councils as victims. exogenous_override_reading authors higher epsilon for the same regime read as coerced abandonment, expanding the victim set toward the entire covenant community's surrendered divine requirement. institutional_pragmatism_reading authors epsilon for a self-dealing adaptation, collapsing the beneficiary set onto the institution alone. The endogenous reading is upstream in legitimacy terms: it is the institution's official account and supplies the revelatory vocabulary the other two readings contest; each family member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
