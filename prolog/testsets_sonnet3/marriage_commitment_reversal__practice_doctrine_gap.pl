% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Doctrine-Practice Gap in Plural Marriage After the 1890 Manifesto
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story reads the 1890 Manifesto and its aftermath not as a clean
 *   doctrinal reversal (the endogenous-revelation reading) nor as pure
 *   external coercion leaving doctrine untouched (the exogenous-override
 *   reading), but as a structural ambiguity deliberately maintained: Section
 *   132 was never formally rescinded as scripture, while public compliance
 *   with federal anti-polygamy pressure was declared. This gap between
 *   preserved doctrine and suspended practice is the constraint itself. It is
 *   what allowed the institution to tell federal officials one story and
 *   believing members another, and it is what enabled over 200 additional
 *   plural marriages to be performed or authorized between 1890 and 1904 in
 *   jurisdictions the institution claimed were outside U.S. legal reach,
 *   while simultaneously presenting a face of full compliance domestically.
 *   This is a distinct constraint from a claim about the source of the
 *   reversal (revelation vs. coercion) — this story's ε concerns the cost of
 *   maintaining the gap itself, regardless of why the gap opened.
 *
 * KEY AGENTS:
 *   - institutional_hierarchy: sets and maintains the ambiguous public/private doctrine split, primary strategic beneficiary
 *   - general_membership: bears the confusion and inconsistent enforcement produced by the gap
 *   - fundamentalist_believers: act on the preserved doctrine literally and are cast out for it
 *   - plural_wives_and_children_post_manifesto: exist in a legal and doctrinal no-man's-land the gap created
 *   - federal_officials: accept the public-facing compliance narrative, enabling the gap to function externally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.78).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.61).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Doctrine-Practice Gap in Plural Marriage After the 1890 Manifesto").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '5d41ddda-f853-440d-89a3-123d120e9c5e').
narrative_ontology:cs_kernel_codification('5d41ddda-f853-440d-89a3-123d120e9c5e', fixed_text).
narrative_ontology:cs_authority_grounding('5d41ddda-f853-440d-89a3-123d120e9c5e', extraction).
narrative_ontology:cs_interpretation_layer_present('5d41ddda-f853-440d-89a3-123d120e9c5e').
narrative_ontology:cs_reading_relation('5d41ddda-f853-440d-89a3-123d120e9c5e', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('5d41ddda-f853-440d-89a3-123d120e9c5e', marriage_commitment_reversal__endogenous_reinterpretation_reading, influences).
narrative_ontology:cs_axiom('5d41ddda-f853-440d-89a3-123d120e9c5e', foundational, unresolved_ambiguity_is_itself_the_operative_structure).
narrative_ontology:cs_axiom_status(unresolved_ambiguity_is_itself_the_operative_structure, holdable).
narrative_ontology:cs_axiom_grounding('5d41ddda-f853-440d-89a3-123d120e9c5e', unresolved_ambiguity_is_itself_the_operative_structure, conventional).
narrative_ontology:cs_axiom('5d41ddda-f853-440d-89a3-123d120e9c5e', secondary, dual_audience_legitimation_is_sustainable_indefinitely).
narrative_ontology:cs_axiom_status(dual_audience_legitimation_is_sustainable_indefinitely, holdable).
narrative_ontology:cs_axiom_grounding('5d41ddda-f853-440d-89a3-123d120e9c5e', dual_audience_legitimation_is_sustainable_indefinitely, instrumental).
narrative_ontology:cs_reference_frame('5d41ddda-f853-440d-89a3-123d120e9c5e', section_132_eternal_covenant_framework).
narrative_ontology:cs_drift_state('5d41ddda-f853-440d-89a3-123d120e9c5e', post_manifesto_dual_track_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5d41ddda-f853-440d-89a3-123d120e9c5e', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, church_survival_and_statehood_interests).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_believers).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, plural_wives_and_children_post_manifesto).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the public Manifesto suspending plural marriage while never repudiating Section 132 as scripture, and quietly authorizes or tolerates over 200 additional plural marriages between 1890 and 1904 in Mexico, Canada, and elsewhere claimed to be outside U.S. jurisdiction. Controls which reading of the reversal is presented to which audience — divine revelation to the faithful, practical compliance to federal officials — and bears no personal cost for the ambiguity it maintains.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, institutional_hierarchy, beneficiary).

% Told simultaneously that God's law of celestial marriage is eternal and unchanged, and that the practice has ended by revelation. Experiences confusion and a sense of betrayal as some members are permitted to continue plural marriages quietly while the institution disciplines or excommunicates others for the same conduct. Has no reliable way to know which behavior is actually sanctioned, since the doctrine says one thing and enforcement says another depending on visibility and location.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    powerless, biographical, constrained, national).

% Take Section 132's preserved doctrinal status literally — as evidence the principle was never actually revoked, only suspended for public relations — and continue or attempt to continue plural marriage as a matter of religious obligation. Are excommunicated and driven into schismatic communities for acting on the very doctrine the institution never disavowed. Bear the full cost of the ambiguity the hierarchy created and sustains.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_believers, payer,
    powerless, generational, trapped, regional).

% Enter or remain in plural marriages performed quietly after 1890 under continued institutional sanction, then face legal invisibility, social stigma, and disavowal by the same institution that authorized the unions, as the public narrative of a clean 1890 break hardens over time. Their marriages exist in a jurisdictional and doctrinal no-man's-land the institution created and can disclaim at will.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, plural_wives_and_children_post_manifesto, payer,
    powerless, biographical, trapped, regional).

% The abstract institutional interest in Utah statehood, federal amnesty, and property restoration is served by a public posture of compliance that does not require doctrinal capitulation. This interest collects the primary benefit of the ambiguous reversal without being a discrete decision-making agent — it names what the hierarchy's maneuvering room actually purchases.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, church_survival_and_statehood_interests, beneficiary,
    institutional, civilizational, arbitrage, national).

% Accept the public Manifesto as sufficient evidence of compliance to proceed with statehood and amnesty processes, largely without investigating the continued authorization of marriages outside claimed jurisdiction. Their acceptance of the public-facing reading is part of what makes the ambiguity structurally viable.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_officials, observer,
    institutional, biographical, analytical, national).

% Internal voices who argued the institution should either formally revoke Section 132's doctrinal status or openly defend continued practice, rather than maintain the gap, were marginalized from the official record and from decision-making channels; their position — that the ambiguity itself was the injury — was never adjudicated.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, excluded_dissenting_theologians, excluded,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, institutional_hierarchy).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution a mechanism to present externally-required compliance (ending plural marriage publicly) while preserving internal doctrinal continuity (Section 132 remains canonical scripture), avoiding the destabilizing admission that a divinely-mandated principle was simply wrong or coerced away.
% TRANSFER_FUNCTION: Moves the cost of doctrinal incoherence from the institution, which retains flexibility and legitimacy with both federal authorities and believing members, onto ordinary members who cannot reconcile competing signals, and onto fundamentalists and post-Manifesto plural families who act on the preserved doctrine and are then disowned by the same body that quietly permitted or performed their marriages.
% ABSENT_VOICES: Members who continued or entered plural marriages 1890-1904 under quiet authorization, and internal theologians who wanted the ambiguity resolved one way or the other, were not part of the official narrative-setting process; their experience is documented mainly in genealogical and legal records rather than institutional history.
% DISAPPEARANCE_RATIONALE: If the doctrine-practice gap were resolved — either by formally revoking Section 132 or by openly defending continued practice — the institution would lose either its claim to unbroken revelatory continuity or its claim to federal-compliant modernity. Fundamentalist schismatic movements exist specifically because the gap was never closed; closing it retroactively would either validate or delegitimize them outright, reorganizing decades of institutional and dissident identity.
% FOUNDING_PROBLEM: The institution faced simultaneous federal prosecution, property confiscation, and a path to statehood contingent on ending plural marriage, while its foundational scripture (Section 132) declared the practice an eternal requirement for the highest degree of celestial glory — a problem that could not be solved by simple compliance without appearing to admit prior revelation was false.
% FOUNDING_PROBLEM_CORROBORATION: Federal officials and legal historians attest the practical problem (federal seizure, statehood blockage) was resolved by 1904-1907 as effective enforcement ended. Fundamentalist communities and independent historians of Mormon polygamy attest the doctrinal problem was never resolved, only suspended, citing the continued canonical status of Section 132 and documented post-Manifesto marriages as evidence the gap was a deliberate, ongoing institutional strategy rather than a completed transition.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) and rising across the interval because the ambiguity is not a transitional artifact that resolves — it is actively exploited for institutional benefit (continued marriages, dual legitimation) for over a decade, then hardens into permanent unresolved doctrine rather than being cleaned up. Theater ratio is substantial (0.58) because much of the public Manifesto's function is performative compliance signaling to federal audiences, decoupled from actual practice on the ground. Suppression (0.61) reflects the real disciplinary and social enforcement mechanisms used against those who read the doctrine literally (excommunication of fundamentalists) alongside the institution's own freedom from any equivalent constraint. accessibility_collapse is moderate (0.42), not near-mountain levels, because alternative resolutions (formal revocation, open defense) were visibly available and argued for internally — the gap was a choice, not an inevitability. Resistance is high (0.72): fundamentalist schism, internal theological dissent, and later public scandal (the Smoot hearings) all constitute active resistance to the ambiguity's persistence.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional hierarchy's seat, the gap is a survival mechanism — necessary flexibility during an existential crisis, later normalized as historical nuance. From the fundamentalist and general membership seats, the same structure computes as betrayal: told the principle was eternal, then told it ended, with no honest accounting of which claim controls their own conduct. The engine's per-seat computation should show these seats landing in structurally different places even though they inhabit the same nominal institution.
 *
 * DIRECTIONALITY LOGIC:
 *   institutional_hierarchy sits near the full-beneficiary end: it authored the ambiguity, controls both audiences it is presented to, and bears none of the disciplinary consequences it enforces on others. general_membership and fundamentalist_believers sit near the full-target end: they are structurally trapped by identity and community ties (exit_options trapped/constrained) and pay the cost of a contradiction they did not create and cannot resolve on their own authority. plural_wives_and_children_post_manifesto are the most acute targets: their very legal and family status depends on a determination the institution can make and unmake at will.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal prosecution threatening survival and statehood) was substantially resolved by the early 1900s through effective compliance and eventual amnesty. But the doctrine-practice gap that solved that problem was never closed down once its founding purpose was achieved — Section 132 remains canonical to this day. This is the mandatrophy signature: an arrangement whose founding problem (survival crisis) is dead, but which persists as unresolved structural ambiguity because closing it in either direction (revoke or restore) carries costs the institution has never been willing to bear. Classifying this as tangled_rope rather than pure snare preserves the genuine coordination function it once served (surviving an existential legal threat) while still registering the asymmetric extraction that persists after that function's expiration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gap_deliberate_vs_emergent,
    'Was the doctrine-practice gap a deliberately engineered institutional strategy, or an emergent byproduct of genuine internal disagreement about how far the Manifesto''s revelation extended?',
    'Close archival analysis of First Presidency correspondence and minutes 1890-1904 regarding authorized post-Manifesto marriages, compared against public statements to federal officials in the same period, to establish whether the dual-track posture was coordinated policy or uncoordinated drift.',
    'If deliberately engineered, this reading''s tangled_rope classification with high extractiveness is well-supported — the ambiguity was manufactured for institutional benefit. If emergent from genuine internal disagreement without central coordination, the constraint may be better read as an unresolved scaffold that failed to sunset, with less concentrated agency at the beneficiary seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gap_deliberate_vs_emergent, empirical, 'Whether the practice-doctrine gap was strategic institutional design or unintentional drift.').

omega_variable(
    gap_vs_mechanism_readings_independence,
    'Can the practice_doctrine_gap reading''s claims about the ambiguity''s cost be fully independent of which mechanism (revelation or coercion) the sibling readings identify as triggering the 1890 reversal, or does the gap''s very existence depend on which mechanism is true?',
    'Structural test: would the gap and its extraction pattern exist under either sibling mechanism? If the ambiguity''s function (dual legitimation, differential enforcement) is present regardless of whether Woodruff''s vision or federal coercion is the accepted trigger, the readings are genuinely independent constraints, not restatements of one fact.',
    'If dependent, this story''s ε may need coupling to the mechanism readings'' resolution; if independent (as authored here), the three readings remain properly decomposed per the ε-invariance principle, each with a stable ε describing a different structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gap_vs_mechanism_readings_independence, conceptual, 'Whether the gap-cost claim is structurally separable from the mechanism-of-reversal claims in the sibling readings.').

omega_variable(
    fundamentalist_schism_scale,
    'What proportion of total membership loss to fundamentalist schism between 1890 and 1935 is attributable specifically to the preserved-doctrine ambiguity, versus other factors (frontier isolation, charismatic leadership of splinter figures, unrelated theological disputes)?',
    'Historical demographic analysis of schismatic community formation dates cross-referenced against periods of heightened institutional crackdown on quiet post-Manifesto marriages.',
    'A high attributable proportion strengthens the victim classification of fundamentalist_believers as directly harmed by this specific constraint rather than by broader sectarian dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fundamentalist_schism_scale, empirical, 'How much of the fundamentalist schism is causally tied to the doctrine-practice gap specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1894, 0.5).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1898, 0.58).
narrative_ontology:measurement(marr_tr_t1902, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1902, 0.63).
narrative_ontology:measurement(marr_tr_t1906, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1906, 0.6).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1910, 0.58).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1894, 0.68).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1898, 0.74).
narrative_ontology:measurement(marr_be_t1902, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1902, 0.79).
narrative_ontology:measurement(marr_be_t1906, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1906, 0.81).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1910, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1894, 0.58).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1898, 0.63).
narrative_ontology:measurement(marr_su_t1902, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1902, 0.66).
narrative_ontology:measurement(marr_su_t1906, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1906, 0.62).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1910, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__practice_doctrine_gap, 0.08).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the marriage_commitment_reversal kernel. exogenous_override_reading treats the 1890 shift as pure external coercion with doctrine held static; endogenous_reinterpretation_reading treats it as a completed internal revelatory event; this story (practice_doctrine_gap) treats the persisting ambiguity BETWEEN preserved doctrine and suspended practice as itself the operative constraint, with its own distinct ε (0.78, high) reflecting sustained extraction from the gap's maintenance rather than from either the coercion event or the revelatory event alone. All three should be read together as a constraint family; none supersedes the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
