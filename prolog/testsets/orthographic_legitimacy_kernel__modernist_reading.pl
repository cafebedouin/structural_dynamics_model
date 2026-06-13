% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: Orthographic Script Change as Modernist National Identity Transformation
 *   domain: political/linguistic/commitment_systems
 *
 * SUMMARY:
 *   The modernist reading frames the script change as a constitutive identity
 *   transformation and alignment with Western modernity, necessarily
 *   rupturing from the Ottoman/Islamic past. Under this reading, the
 *   constraint operates as a Tangled Rope: genuine coordination function
 *   (unified literacy, mass education enablement, administrative
 *   standardization) coupled with asymmetric extraction from traditional
 *   elites (rendering Ottoman literacy economically worthless, severing
 *   religious scholars from textual authority, displacing cultural
 *   custodians). The extractiveness is high because the script change is not
 *   merely instrumental to literacy but ideological—it mandates rupture from
 *   a past frame as the condition of legitimate citizenship in the modernized
 *   nation. Suppression is substantial because maintaining the constraint
 *   requires actively preventing the old script from retaining legitimacy,
 *   barring its use in official contexts, and treating it as a marker of
 *   backwardness. The theater ratio rises over the interval as the constraint
 *   matures: early on, security and efficiency justifications are genuine;
 *   later, the main enforcement burden shifts to defending the rupture
 *   narrative itself against historical memory and competing readings.
 *
 * KEY AGENTS:
 *   - modernizing_state_apparatus: Sets and enforces the script mandate; collects international legitimacy and eliminates textual competition.
 *   - ottoman_literate_elite: Bears extraction via obsolescence of their accumulated knowledge; identity-locked by decades of training in the displaced script.
 *   - religious_scholars: Structurally excluded from defining legitimacy; their epistemic authority severs with the script change; they can oppose (marked as backward) or accept marginalization.
 *   - youth_cohort: Beneficiaries; experience the new script as natural; carriers of the modernist national identity frame.
 *   - international_modernist_peers: Recognize and reward the state's alignment with Western modernity; provide validation and positioning.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.81).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.76).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "Orthographic Script Change as Modernist National Identity Transformation").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political/linguistic/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, 'ddbefcae-5112-44e7-8de1-0672a1fcded6').
narrative_ontology:cs_kernel_codification('ddbefcae-5112-44e7-8de1-0672a1fcded6', formalized).
narrative_ontology:cs_authority_grounding('ddbefcae-5112-44e7-8de1-0672a1fcded6', extraction).
narrative_ontology:cs_interpretation_layer_present('ddbefcae-5112-44e7-8de1-0672a1fcded6').
narrative_ontology:cs_reading_relation('ddbefcae-5112-44e7-8de1-0672a1fcded6', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('ddbefcae-5112-44e7-8de1-0672a1fcded6', orthographic_legitimacy_kernel__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('ddbefcae-5112-44e7-8de1-0672a1fcded6', foundational, modernist_identity_requires_orthographic_rupture).
narrative_ontology:cs_axiom_status(modernist_identity_requires_orthographic_rupture, holdable).
narrative_ontology:cs_axiom_grounding('ddbefcae-5112-44e7-8de1-0672a1fcded6', modernist_identity_requires_orthographic_rupture, deontological).
narrative_ontology:cs_axiom('ddbefcae-5112-44e7-8de1-0672a1fcded6', secondary, ottoman_past_incompatible_with_civilizational_progress).
narrative_ontology:cs_axiom_status(ottoman_past_incompatible_with_civilizational_progress, holdable).
narrative_ontology:cs_axiom_grounding('ddbefcae-5112-44e7-8de1-0672a1fcded6', ottoman_past_incompatible_with_civilizational_progress, empirically_contingent).
narrative_ontology:cs_reference_frame('ddbefcae-5112-44e7-8de1-0672a1fcded6', ottoman_islamic_orthographic_tradition).
narrative_ontology:cs_drift_state('ddbefcae-5112-44e7-8de1-0672a1fcded6', post_modernization_alignment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ddbefcae-5112-44e7-8de1-0672a1fcded6', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_elite).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, religious_scholars).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, traditional_cultural_custodians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, youth_cohort).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, international_modernist_peers).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, western_modernity_as_inevitable_trajectory).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, script_change_as_civilizational_rupture).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, ottoman_past_as_obstacle_to_progress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the script mandate via education policy, administrative decree, and legal framework. Controls the new curriculum, examinations, and official language standards. Justifies the change as alignment with Western modernity and scientific progress. Collects political legitimacy from international recognition as a modern nation-state and eliminates competing sources of authority (traditional textual custodians and religious scholars). The constraint's persistence depends on the state apparatus maintaining the rupture narrative and preventing alternatives from gaining institutional legitimacy.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Rendered functionally illiterate despite decades of training in Ottoman orthography. Accumulated cultural capital (mastery of Ottoman texts, calligraphy, literary forms, legal documents) becomes economically worthless. Cannot relearn at advanced age without substantial cost and social dislocation. Knowledge of Islamic, Persian, and Ottoman literary traditions is treated as belonging to a dead past by state-controlled institutions. Social status, employment options, and institutional power collapse. Trapped by identity fusion with the displaced script—to accept the modernist frame is to accept their own obsolescence.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_elite, payer,
    powerful, biographical, identity_locked, national).

% Entire epistemic authority rested on mastery of religious texts in the old script. Script change severs them from the Quran, Hadith, and centuries of Islamic jurisprudence via state-mandated education that does not teach the old script. Forced to either oppose (marked as obstacles to progress) or accept marginalization. Institutional power in courts, education, and moral authority erodes as the state monopolizes legitimate literacy. Structurally excluded from setting the new legitimacy frame; any appeal to tradition is reframed as backwardness.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, religious_scholars, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, religious_scholars, excluded).

% Poets, historians, calligraphers, and cultural historians whose entire practice is grounded in the old orthography and its aesthetic, historical, and spiritual meanings. Face a choice: preserve old forms in a shrinking, underground cultural space (risking social marginalization and loss of institutional support), or migrate to the new script (losing historical continuity and aesthetic coherence). Cultural practice becomes marked as backwardness in state-controlled discourse.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, traditional_cultural_custodians, payer,
    moderate, generational, identity_locked, national).

% Educated exclusively in the new script; they are the first cohort for whom it is native literacy. Gain access to state education, printed materials, and administrative opportunity routes closed to previous generations. Experience the script change as natural—as how literacy simply is. Become carriers of the modernist national identity frame. Isolated from historical texts in the old script; historical memory becomes mediated through state-controlled translation and interpretation. Constrained by the elimination of the old script from official spaces, but this constraint is invisible to them because it aligns with their native literacy.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, youth_cohort, beneficiary,
    powerless, biographical, constrained, national).

% Western and European states recognize the script change as a marker of civilization and alignment with progress. The adopting state gains international legitimacy, access to technical cooperation, and positioning as a modern nation-state. International standards organizations, educational institutions, and diplomatic recognition flow to the newly aligned state. The rupture from Ottoman/Islamic identity becomes a legitimacy asset in the international order.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, international_modernist_peers, beneficiary,
    institutional, generational, analytical, global).

% Other linguistic or ethnic communities within or adjacent to the state may resist or resent the script change if it erases their own orthographic traditions or forces adoption of the dominant group's modernist frame. Their own scripts may be treated as markers of backwardness. Structurally excluded from setting the terms of what counts as civilizational progress; any appeal to cultural preservation is reframed as resistance to modernization.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, competing_ethnic_or_linguistic_communities, excluded,
    organized, generational, trapped, regional).

% Retrospectively analyze the constraint's operation: whether it was necessary for literacy, whether it severed genuine cultural knowledge, whether the modernist frame was intrinsically required or politically imposed, whether international legitimacy truly depended on script change or whether that was post-hoc rationalization. They assess the reading's coherence and the gap between the constraint's claimed and actual functions.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, historians_and_literary_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__modernist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, unified, state-standardized orthographic system to replace multiple competing historical scripts, enabling mass education, standardized printing and typography, administrative efficiency, and a shared written culture for national political integration. Creates one common literacy standard across a diverse population and eliminates the fragmentation that characterized late Ottoman administration.
% TRANSFER_FUNCTION: Transfers cultural authority and epistemic legitimacy from traditional text-custodians and religious scholars to the state apparatus and international modernist framework. Renders one form of literacy (Ottoman script mastery) economically worthless; requires retraining or obsolescence of knowledge-workers trained in the prior system. Transfers the right to define what counts as civilization from local historical tradition to Western/European alignment. Extracts status and institutional power from traditional elites and consolidates it in the modernizing state apparatus and international peers.
% ABSENT_VOICES: Religious scholars are structurally excluded from the conversation; they might argue the script change severs a living, theologically grounded connection to centuries of Islamic jurisprudence and philosophical tradition, and that forced severance is a form of cultural violence masquerading as progress. Competing ethnic communities with their own orthographic traditions, if present, would argue the modernist frame erases their writing systems and imposes a dominant group's identity project while framing it as universal progress. Ottoman-trained jurists and historians would attest that the old script system was functionally adequate for its purposes and that claims of civilizational superiority are post-hoc. None of these parties are in the room when the script decision is made.
% DISAPPEARANCE_RATIONALE: If the script mandate vanished and the old orthography retained institutional legitimacy, the literate elite would retain their accumulated cultural capital, religious and legal authority would remain grounded in accessible texts, the state would lose the international positioning it gained from modernist alignment, and youth education would not undergo wholesale reorientation toward rupture. The cultural economy would reorganize around competing scripts and their associated knowledge domains. Historical scholarship would be more accessible to practitioners of the traditional fields. The state's claim to civilizational progress would lose one of its primary markers.
% FOUNDING_PROBLEM: The late Ottoman state faced multiple orthographic and literacy challenges: (1) administrative fragmentation—Ottoman, Arabic, Greek, Armenian, Hebrew, and other scripts coexisted within the empire, with educated classes divided by script; (2) Ottoman administrative and literary texts were inaccessible to populations educated in different scripts, creating institutional inefficiency; (3) European powers were advancing in technical, scientific, and industrial domains, and Ottoman elites perceived a 'civilizational gap'—a sense that Ottoman educational and administrative systems were failing to keep pace with Western modernization. Script unification was framed as solving both the internal coordination problem (fragmentation) and the civilizational positioning problem (alignment with the modern West).
% FOUNDING_PROBLEM_CORROBORATION: The modernizing state apparatus attests the problem was live and script change was the necessary solution that enabled national integration, mass education, and modernization. International modernist peers attest that the state's alignment with Western orthographic standards marked it as a civilized, progressive nation-state. Historians of Ottoman administrative efficiency attest that standardization improved cross-institutional communication. However, historians of linguistic colonialism and scholars of Ottoman cultural history note that (1) the perceived 'civilizational gap' was partly a frame imposed and reinforced by Western powers—Ottoman literacy was genuinely functional for its purposes; (2) multiple scribal systems in the Ottoman empire had coexisted and competed for centuries without creating the administrative crisis claimed; (3) the script change was as much about erasing Ottoman identity and Islamic continuity as about solving a coordination problem; (4) European languages also underwent standardization without wholesale script rupture. No external party outside the benefiting institutions (state apparatus, international peers) corroborates that script change was NECESSARY; corroboration is limited to those who benefit from the rupture narrative.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).

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
 *   The extractiveness trajectory (0.65 → 0.81) reflects the constraint's deepening entrenchment: early implementations focus on education and administration (coordination is real); over 50 years, the constraint's main burden becomes defending the rupture narrative against historical scholarship and the lived memory of what was lost. The suppression requirement rises in parallel (0.58 → 0.76) because the constraint's persistence requires actively preventing the old script from being taught, used in courts, or preserved in official archives. Theater rises more slowly (0.25 → 0.42) because the constraint's coordination function is genuine enough that theatrical elements remain a minority; but as the founding problem (literacy fragmentation, civilizational gap) becomes less pressing, enforcement increasingly maintains the rupture narrative rather than solving the original problem. The accessibility collapse is moderate (0.72) because alternatives do exist—other states use different scripts, religious communities preserve the old texts privately, scholars can still learn the old orthography—but the state apparatus makes all alternatives inconvenient, costly, or socially marked as backward.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (literate elite, religious scholars) and the agenda-setter seat (state apparatus) should compute very differently. The state apparatus perceives itself as solving a coordination problem and enabling progress. The literate elite perceive themselves as victims of a politically motivated erasure. Religious scholars perceive themselves as structurally excluded from setting the terms of legitimacy. The engine computes this from power + exit + beneficiary/victim declarations; this is where the classification gap should appear—the state sees rope, the victims see snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernizing state apparatus is the structural beneficiary: it sets the rules, controls education, defines what counts as legitimate, and collects international legitimacy. Directionality for this agent is near 0.0 (full beneficiary). The ottoman_literate_elite and religious_scholars are the targets: they bear the extraction (obsolescence, authority loss, cultural displacement); their directionality approaches 1.0 (full target). Identity-locking makes exit nearly impossible for these groups—they cannot simply switch scripts at age 50 and retain their social position. The youth_cohort sits near 0.3-0.4 (net beneficiary but carry diffuse costs): they gain access and integration but lose connection to historical texts and cultural memory. International peers sit near 0.0 (beneficiary): they gain a modernized partner and reduced 'civilizational gap' pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy classification (which would apply if the founding problem was dead but the constraint persisted by pure inertia) because the modernist reading asserts that alignment with Western modernity IS the ongoing founding problem. Under this reading, the constraint persists not as theater but as active identity maintenance. However, a competing reading—the instrumentalist reading—would argue the founding problem (administrative efficiency, mass literacy) is substantially solved and the constraint now persists mostly by theater (defending the rupture narrative). The mandatrophy question hinges on which reading is correct: does the modernist frame remain a live founding problem, or has it become a post-hoc cover story? This ambiguity is captured in the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modernist_frame_intrinsic_vs_imposed,
    'Is the modernist reading''s framing of the script change as civilizational rupture an intrinsic necessity of the constraint''s operation, or is it an imposed interpretive layer added post-hoc for political legitimacy?',
    'Historical analysis of decision documents from the script-change period: do they frame it as identity-constitutive or as instrumental to literacy/efficiency? Do contemporary accounts from benefiting elites differ from later historical narratives? Post-implementation analysis: does the constraint''s persistence require active defense of the rupture narrative, or would it persist on literacy/efficiency grounds alone?',
    'If intrinsic: the constraint is a genuine tangled rope with real coordination function embedded in identity transformation. If imposed: the constraint is closer to a snare using a coordination cover story; the extraction from traditional elites is not necessary to solving the founding problem but serves the modernizing state''s political project of erasing Ottoman identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_frame_intrinsic_vs_imposed, conceptual, 'Whether the modernist frame (rupture from Ottoman past) is constitutive or post-hoc rationalization.').

omega_variable(
    script_change_necessity,
    'Was the script change necessary to achieve the founding problem''s solution (literacy, administrative efficiency, national integration), or could the same outcomes have been achieved by standardizing the Ottoman script instead?',
    'Counterfactual historical analysis: examine cases where literacy and administrative efficiency were achieved WITHOUT script rupture (Japan, China). Examine the technical properties of Ottoman vs. Latin scripts for typography, printing, and modern text processing. Compare literacy rates in states that standardized existing scripts vs. adopted foreign scripts.',
    'If script change was necessary: the constraint is genuinely problem-solving and the extraction is an unavoidable cost of the coordination function. If unnecessary: the constraint is extractive beyond what problem-solving required; the rupture narrative becomes the primary extraction mechanism, not a side effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(script_change_necessity, empirical, 'Whether script change was necessary or whether Ottoman script standardization would have solved the founding problem.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.76 at interval end) structural (state apparatus actively prevents old script use in courts, education, administration) or internalized (populations have accepted the rupture narrative and no longer believe the old script is legitimate)?',
    'Post-suppression removal: if state enforcement of the script mandate ceased, would populations spontaneously preserve the old script, or has it become genuinely unmarked/invisible? Survey data on whether young people view the old script as culturally valuable vs. backward. Comparison with states that maintained script pluralism: do populations there experience both scripts as legitimate, or has one become marked as foreign?',
    'If structural: the constraint persists through ongoing state enforcement; removing the enforcement would enable the old script to re-emerge. If internalized: the suppression has become self-perpetuating; removal of enforcement would not restore the old script because populations have genuinely adopted the modernist frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized into the modernist national identity.').

omega_variable(
    reading_foreclosure_status,
    'Does the modernist reading logically foreclose the continuity reading (that legitimacy derives from preserving tradition), or do they coexist as live but opposed readings held by different parties?',
    'Examination of whether a single party can hold both readings without logical contradiction (coexists) or whether accepting the modernist axiom necessarily rejects the continuity axiom (forecloses). Current discourse analysis: are there parties attempting to hold both readings, or has the divide hardened into two mutually exclusive camps?',
    'If foreclosure: the modernist reading is a stronger claim—it asserts that rupture from the past is NECESSARY for legitimacy, not merely one option among several. This strengthens the extraction narrative (the state is not choosing between options; it is enforcing an axiom). If coexistence: the readings remain in live dispute and the constraint''s legitimacy remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_status, conceptual, 'Whether the modernist reading logically forecloses the continuity reading or whether both remain live positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t5, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(orth_tr_t5, observed).
narrative_ontology:measurement(orth_tr_t10, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(orth_tr_t10, observed).
narrative_ontology:measurement(orth_tr_t20, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(orth_tr_t20, observed).
narrative_ontology:measurement(orth_tr_t35, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement_basis(orth_tr_t35, observed).
narrative_ontology:measurement(orth_tr_t50, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(orth_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t5, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 5, 0.69).
narrative_ontology:measurement_basis(orth_be_t5, observed).
narrative_ontology:measurement(orth_be_t10, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement_basis(orth_be_t10, observed).
narrative_ontology:measurement(orth_be_t20, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(orth_be_t20, observed).
narrative_ontology:measurement(orth_be_t35, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 35, 0.79).
narrative_ontology:measurement_basis(orth_be_t35, observed).
narrative_ontology:measurement(orth_be_t50, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 50, 0.81).
narrative_ontology:measurement_basis(orth_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(orth_su_t0, observed).
narrative_ontology:measurement(orth_su_t5, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(orth_su_t5, observed).
narrative_ontology:measurement(orth_su_t10, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(orth_su_t10, observed).
narrative_ontology:measurement(orth_su_t20, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(orth_su_t20, observed).
narrative_ontology:measurement(orth_su_t35, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 35, 0.74).
narrative_ontology:measurement_basis(orth_su_t35, observed).
narrative_ontology:measurement(orth_su_t50, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement_basis(orth_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__modernist_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__instrumentalist_reading).

% DUAL FORMULATION NOTE:
% The orthographic legitimacy kernel decomposes into three structurally distinct readings, each instantiating a different constraint with different ε values, different beneficiary/victim structures, and different extraction mechanisms. The modernist_reading (this story) frames script change as constitutive of national identity and achieves ε ≈ 0.81 via identity-based extraction from traditional elites. The continuity_reading treats the old script as a living cultural-historical commitment with much lower extraction (those who preserve it do not face state suppression as intensely in that reading's framing). The instrumentalist_reading focuses on literacy/efficiency gains and treats the script change as a pragmatic coordination mechanism, reducing the extraction narrative to collateral damage rather than the point. Each reading has its own kernel_codification, authority_grounding, reading_relations, and axioms; they are linked here via network.affects_constraints because the modernist reading's assertion that rupture is necessary influences (and partly forecloses) the continuity reading's ability to claim legitimacy in state institutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__modernist_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
