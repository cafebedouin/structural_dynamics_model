% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__modernist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: 1928 Turkish Latin Alphabet Reform as Civilizational Rupture (Modernist Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This story generates the MODERNIST reading of a contested kernel: the
 *   claim that orthographic legitimacy for the new Turkish state derives
 *   specifically from alignment with Western/European modernity and rupture
 *   from the Ottoman/Islamic past — not from preserving access to tradition
 *   (continuity_reading) and not primarily from maximizing literacy or
 *   administrative efficiency (instrumentalist_reading), though this reading
 *   does not deny that literacy gains occurred alongside the rupture. Under
 *   this reading, the script change is constitutive of a national identity
 *   transformation: the Arabic-to-Latin alphabet reform (1928) is read as a
 *   deliberate act of civilizational reorientation whose value to the state
 *   lies precisely in its irreversibility and its symbolic severance from the
 *   Ottoman-Islamic literate and religious order, not merely in its
 *   administrative convenience. The extraction this reading measures — a
 *   rapid, enforced devaluation of an entire class's professional and
 *   religious-institutional capital — is treated as a feature of the rupture
 *   project, not an unfortunate side effect of a purely efficiency-driven
 *   reform.
 *
 * KEY AGENTS:
 *   - modernizing_state_apparatus: agenda_setter/beneficiary — decrees, enforces, and narrates the rupture
 *   - new_latin_literate_cadre: beneficiary — inherits access and status vacated by the old literate class
 *   - secular_republican_elite: beneficiary — legitimacy narrative depends on the rupture's visibility
 *   - ottoman_literate_class: payer — professional capital rendered obsolete
 *   - religious_scholars_ulema: payer — epistemic authority severed from state-recognized script
 *   - older_generation_arabic_script_users: payer — functionally reclassified as illiterate
 *   - diaspora_and_minority_script_communities: excluded — assimilated into the same project without voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.78).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.81).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "1928 Turkish Latin Alphabet Reform as Civilizational Rupture (Modernist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, '3e4a9879-64d5-4038-b71c-8a6d919034a9').
narrative_ontology:cs_kernel_codification('3e4a9879-64d5-4038-b71c-8a6d919034a9', formalized).
narrative_ontology:cs_authority_grounding('3e4a9879-64d5-4038-b71c-8a6d919034a9', extraction).
narrative_ontology:cs_interpretation_layer_present('3e4a9879-64d5-4038-b71c-8a6d919034a9').
narrative_ontology:cs_reading_relation('3e4a9879-64d5-4038-b71c-8a6d919034a9', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('3e4a9879-64d5-4038-b71c-8a6d919034a9', orthographic_legitimacy_kernel__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('3e4a9879-64d5-4038-b71c-8a6d919034a9', foundational, civilizational_rupture_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(civilizational_rupture_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3e4a9879-64d5-4038-b71c-8a6d919034a9', civilizational_rupture_constitutes_legitimacy, conventional).
narrative_ontology:cs_axiom('3e4a9879-64d5-4038-b71c-8a6d919034a9', secondary, ottoman_islamic_past_is_disqualifying_inheritance).
narrative_ontology:cs_axiom_status(ottoman_islamic_past_is_disqualifying_inheritance, holdable).
narrative_ontology:cs_axiom_grounding('3e4a9879-64d5-4038-b71c-8a6d919034a9', ottoman_islamic_past_is_disqualifying_inheritance, conventional).
narrative_ontology:cs_reference_frame('3e4a9879-64d5-4038-b71c-8a6d919034a9', ottoman_islamic_literate_continuity).
narrative_ontology:cs_drift_state('3e4a9879-64d5-4038-b71c-8a6d919034a9', post_1928_alphabet_decree, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('3e4a9879-64d5-4038-b71c-8a6d919034a9', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, new_latin_literate_cadre).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, secular_republican_elite).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, older_generation_arabic_script_users).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, civilizational_westward_reorientation_doctrine).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, script_as_national_rebirth_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the Latin alphabet by decree, establishes Millet Mektepleri (Nation's Schools) to retrain the population, and criminalizes continued official use of Arabic script within a matter of years. Frames the change as the physical instantiation of a rupture from the Ottoman-Islamic past and alignment with Western civilization. Collects legitimacy, administrative control over what counts as literate participation in the new state, and control of the historical narrative going forward.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, beneficiary).

% Young, urban, often state-educated or state-aligned individuals who acquire the new script quickly and are rewarded with access to bureaucratic posts, publishing, and the symbolic status of being modern. Their advancement is structurally tied to the devaluation of the older script economy — they benefit precisely because rivals who hold Arabic-script literacy are locked out.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, new_latin_literate_cadre, beneficiary,
    organized, biographical, mobile, national).

% Uses the script change as one pillar of a broader legitimacy project — the new alphabet is proof-object for the claim that the nation has broken with its Ottoman-Islamic past and joined Western modernity. Their political authority is partly constituted by the visibility and irreversibility of this rupture; a reversal would undermine their founding narrative.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, secular_republican_elite, beneficiary,
    institutional, civilizational, analytical, national).

% Scribes, clerks, and administrators whose professional value was built entirely on Ottoman Turkish in Arabic script. Overnight their functional literacy is rendered obsolete in the eyes of the state; they cannot simply relearn the new system fast enough to preserve their prior standing, and many are pushed out of administrative roles they had spent careers acquiring.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class, payer,
    moderate, biographical, trapped, national).

% Their authority was grounded in exegetical command of Arabic-script texts, Qur'anic recitation, and the religious-legal tradition transmitted in that script. The reform does not merely inconvenience them; it severs the script that carries their epistemic authority from the script the state now recognizes as legitimate, and their institutional position erodes as religious education is marginalized in the new national curriculum.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema, payer,
    moderate, generational, identity_locked, national).

% Ordinary literate adults — merchants, farmers who could read contracts and letters, families who could read religious texts — who become functionally illiterate in the state's eyes within a single decade. Too old, too rurally located, or too economically constrained to attend Millet Mektekpleri and re-acquire full facility; they lose the capacity to read the state's own documents about them.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, older_generation_arabic_script_users, payer,
    powerless, biographical, trapped, national).

% Armenian, Greek, and other minority communities whose own scripts and educational institutions are further marginalized in the same reform wave, folded into an assimilationist national project they had no voice in shaping. Not centered in the modernist rupture narrative at all — their exclusion from the conversation is near-total.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, diaspora_and_minority_script_communities, excluded,
    powerless, biographical, trapped, regional).

% Assess the reform's stated literacy justifications against its demonstrable role in identity engineering — comparing literacy rate trajectories, archival access loss, and the political function of the rupture narrative across decades.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, historians_of_turkish_language_reform, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__modernist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a single, state-recognized script lowers transaction costs in printing, education, and administration by removing the need to maintain parallel Arabic-script and any competing systems — a genuine coordination gain that this reading does not deny.
% TRANSFER_FUNCTION: Moves symbolic and functional literacy-derived authority from the Ottoman-Islamic literate and religious-scholarly classes to a newly created Latin-literate cadre aligned with the state; moves interpretive control over national identity from religious and traditional institutions to the secular state apparatus.
% ABSENT_VOICES: Ulema and Ottoman-trained administrators were not meaningfully consulted on the pace or manner of transition; minority-script communities (Armenian, Greek, Kurdish oral and written traditions) were entirely outside the deliberation. Rural populations who would bear the steepest relearning costs had no organized voice in the decree process.
% DISAPPEARANCE_RATIONALE: Had the modernist framing of the reform (rupture-as-legitimacy) not been imposed — i.e., had script change proceeded on purely instrumentalist grounds without the civilizational-rupture narrative — the Ottoman literate and religious-scholarly classes would likely have retained institutional standing far longer, religious education would not have been as systematically marginalized, and the state's founding legitimacy narrative would rest on different pillars entirely. The rupture narrative is not decorative; removing it changes who holds power.
% FOUNDING_PROBLEM: The new republic needed a legible, teachable, unifying instrument of literacy to accelerate mass education and administrative modernization, and needed a visible, irreversible symbol that the state had broken decisively with the Ottoman-Islamic imperial and religious order it replaced.
% FOUNDING_PROBLEM_CORROBORATION: The state and its successor historiography attest the rupture was necessary and largely completed. Independent linguistic historians and descendants of the displaced Ottoman literate class attest the literacy justification was real but insufficient to explain the totalizing and rapid character of the ban on Arabic script — the speed and criminalization of the old script served an identity-rupture function beyond what literacy gains alone required. No corroboration exists from the ulema or Ottoman administrative class themselves as an organized institutional voice, since that voice was the one being structurally dismantled.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__modernist_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) under this reading because the modernist framing treats the SPEED, TOTALITY, and CRIMINALIZATION of the transition — not merely the change of medium — as the operative mechanism, and that totality is what strips the Ottoman literate and clerical classes of capital they cannot readily rebuild. Suppression is authored even higher and spikes sharply in the early years (0.85-0.90 around years 4-8) reflecting the criminalization of continued Arabic-script use in official contexts and the compressed timeline of the Millet Mektepleri campaign; it eases somewhat in the middle period as the transition normalizes, then rises again as later nationalist historiography re-hardens the rupture narrative as founding myth (year 40). Theater ratio is authored moderate-low and rising (0.10 to 0.28) — the literacy-education function was substantially real early on, but an increasing share of the reform's continued symbolic maintenance (anniversaries, monument culture, curricular emphasis on the 'Alphabet Revolution') is performative reinforcement of the rupture narrative rather than functional literacy work.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernizing state apparatus and the secular republican elite sit at the low-d/beneficiary end: they set the agenda, collect the legitimacy payoff, and bear essentially none of the relearning cost. The new Latin-literate cadre also benefits, with mobile exit options reflecting that their advantage is portable. The Ottoman literate class and the ulema sit at the high-d/target end: trapped or identity-locked, they cannot simply switch scripts back into their prior standing because the state's recognition regime — not their own competence — is what collapsed under them. Older Arabic-script users are powerless and trapped: no institutional capital to leverage and no plausible individual remedy. Minority script communities are excluded rather than coordinated — this reading's rupture narrative was never about them, and their marginalization is a side effect that the modernist narrative does not centrally address.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass illiteracy, administrative fragmentation, absence of a unifying modern instrument) has substantially resolved — literacy under the Latin script is now the unmarked norm and no live administrative crisis argues for reverting. Under the modernist reading specifically, however, the RUPTURE function of the reform — its use as founding-myth material for civilizational reorientation — is a live, ongoing commitment rather than a completed one; the state and its successor institutions continue to invoke it as legitimacy infrastructure decades on. This produces a mismatch worth flagging: founding_problem_status is contested rather than cleanly dead, because the literacy problem is dead but the identity-rupture problem the modernist reading foregrounds is still actively serviced by commemorative and curricular practice — a classic candidate for the mismatch the R5 consumer is built to catch (status effectively dead-for-instrumentalist-purposes, but disappearance_verdict remains world_rearranges because the rupture narrative still structures present political legitimacy claims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_efficiency_separability,
    'Was the civilizational-rupture framing structurally necessary to achieve the literacy and administrative gains, or was it a superimposed identity project riding on an efficiency reform that could have proceeded without the criminalization of the old script and without the rhetoric of Ottoman-Islamic rupture?',
    'Comparative case analysis against other 20th-century script reforms (e.g., Vietnamese quoc ngu adoption, Soviet Central Asian Latinization waves) that pursued literacy gains with varying degrees of explicit civilizational-rupture framing and varying degrees of coercive enforcement against the prior script.',
    'If separable, this reading''s high extraction is attributable specifically to the rupture project rather than to the coordination function, sharpening the case that this is a distinct, more extractive constraint from the instrumentalist reading rather than the same reform under a different gloss. If inseparable, the rupture and the literacy gain may be causally entangled in ways that complicate treating them as fully independent constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_efficiency_separability, conceptual, 'Whether the modernist rupture framing was constitutively necessary to the literacy reform or a separable identity project.').

omega_variable(
    genuine_vs_constructed_civilizational_alignment,
    'Is ''alignment with Western/European modernity'' a coherent, independently specifiable normative target that the reform actually achieved, or is it primarily a legitimating narrative constructed and maintained by the beneficiary state apparatus to justify a domestically extractive redistribution of literate authority?',
    'Track whether the ''Western modernity'' framing was invoked consistently across the reform''s implementation or was selectively deployed at moments when domestic legitimacy needed reinforcement — a consistency-over-time analysis of state rhetoric versus policy substance.',
    'If the alignment target is substantially constructed/selectively deployed, this reading''s classification leans harder toward tangled_rope-with-heavy-extraction (coordination function real but rupture-legitimation largely a cover story). If the alignment target reflects genuine, consistently pursued policy commitments beyond rhetoric, the coordination function is more substantial than the extraction framing alone would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_constructed_civilizational_alignment, conceptual, 'Whether Western-alignment is a genuine achieved target or a constructed legitimating narrative for the state apparatus.').

omega_variable(
    ulema_authority_intrinsic_vs_rent,
    'Was the ulema''s pre-reform authority itself partly extractive (a religious-literate monopoly over legal and educational functions), such that some of what this reading counts as ''extraction from victims'' is better read as removal of the ulema''s own prior extractive position rather than pure victimization?',
    'Historical analysis of ulema institutional privilege and rent-collection under the late Ottoman order, compared against the redistribution pattern after the reform, to assess whether the net effect was extraction-transfer or extraction-reduction-plus-new-extraction.',
    'If the ulema''s prior position was itself substantially extractive, this reading''s victim framing for religious_scholars_ulema requires qualification — the reform may be better modeled as extraction being relocated from one elite to another rather than extraction being newly created. This would not eliminate the tangled_rope classification but would complicate the moral valence commonly attached to it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ulema_authority_intrinsic_vs_rent, conceptual, 'Whether the ulema''s displaced authority was itself extractive, complicating a clean victim framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(orth_tr_t4, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(orth_tr_t8, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(orth_tr_t15, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(orth_tr_t25, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(orth_tr_t40, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(orth_be_t4, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 4, 0.74).
narrative_ontology:measurement(orth_be_t8, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 8, 0.79).
narrative_ontology:measurement(orth_be_t15, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(orth_be_t25, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(orth_be_t40, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(orth_su_t4, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 4, 0.85).
narrative_ontology:measurement(orth_su_t8, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 8, 0.9).
narrative_ontology:measurement(orth_su_t15, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(orth_su_t25, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(orth_su_t40, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__modernist_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from a single natural-language kernel (orthographic_legitimacy_kernel) per the ε-invariance principle: measuring 'orthographic legitimacy' by the rupture-as-identity-transformation observable yields a substantially different ε and beneficiary/victim structure than measuring it by tradition-access-preservation (continuity_reading) or literacy/efficiency-maximization (instrumentalist_reading). The three are linked bidirectionally via affects_constraints rather than merged into one constraint with a measurement parameter, per DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
