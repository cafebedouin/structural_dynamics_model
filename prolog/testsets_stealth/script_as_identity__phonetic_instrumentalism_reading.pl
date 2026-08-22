% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__phonetic_instrumentalism_reading, []).

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
 *   constraint_id: script_as_identity__phonetic_instrumentalism_reading
 *   human_readable: Turkish Latin-Script Orthography as Phonetic Optimization (Phonetic Instrumentalism Reading)
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   A standing arrangement since the 1928 Alphabet Law: Turkish is written in
 *   a Latin-based orthography, mandated in schools, administration, and
 *   print, and maintained by the Turkish Language Association. This story
 *   instantiates ONE reading of the contested script_as_identity kernel, the
 *   phonetic_instrumentalism_reading, which holds that writing systems are
 *   neutral transcription technologies and that the Latin alphabet was
 *   adopted, and is rightly retained, because it transcribes Turkish's
 *   eight-vowel, vowel-harmonic phonology with far greater transparency than
 *   the three-vowel Ottoman abjad. Per the epsilon-referent rule,
 *   extractiveness is authored for the standing arrangement (the mandatory
 *   Latin orthography) as THIS reading assesses it: low, because by the
 *   reading's lights the orthography is a technical optimization whose costs,
 *   the transition generation's relearning, are one-time and amortized. The
 *   sibling readings are separate constraints, not part of this one:
 *   script_as_identity__kemalist_rupture_reading authors the same arrangement
 *   as deliberate civilizational severance enabling secular modernization;
 *   script_as_identity__ottoman_continuity_reading authors the Arabic-script
 *   arrangement as identity-constitutive and the Latin mandate as severance
 *   with a large victim set. The claim/metric gap is deliberate and bounded:
 *   the reading CLAIMS a rope, a phonetically justified coordination
 *   standard, while the authored metrics record a coercive founding
 *   (suppression_requirement 0.78 at t0, decaying to 0.18), a rising share of
 *   performative maintenance (theater 0.12 to 0.30), and a dead founding
 *   problem with prohibitive switching cost. The engine measures that
 *   divergence; this story does not reconcile the claim to the metrics. KEY
 *   AGENTS (by structural relationship): - turkish_language_authority (the
 *   1928 Language Commission and its successor TDK): agenda-setter
 *   (institutional/arbitrage) — codifies and defends the standard -
 *   republican_state_administration: agenda-setter and beneficiary
 *   (institutional/arbitrage) — enacted and enforced the mandate; collects
 *   legibility - turkish_literate_public: primary beneficiary
 *   (moderate/identity_locked) — uses the orthography; locked in by its own
 *   literacy - transition_generation_learners: primary payer
 *   (powerless/trapped) — bore the 1928 relearning coercion -
 *   ottoman_script_literates: excluded voice (moderate/identity_locked) — the
 *   identity objection with no seat in the technical framing -
 *   comparative_linguistics_community: analytical observer — corroborates the
 *   phonetic core, disputes the sufficiency of phonetic determination
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.12).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.18).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, rope).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "Turkish Latin-Script Orthography as Phonetic Optimization (Phonetic Instrumentalism Reading)").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__phonetic_instrumentalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, '8eddb4f7-11a6-424d-8c63-decda5f74b19').
narrative_ontology:cs_kernel_codification('8eddb4f7-11a6-424d-8c63-decda5f74b19', formalized).
narrative_ontology:cs_authority_grounding('8eddb4f7-11a6-424d-8c63-decda5f74b19', expertise).
narrative_ontology:cs_interpretation_layer_present('8eddb4f7-11a6-424d-8c63-decda5f74b19').
narrative_ontology:cs_reading_relation('8eddb4f7-11a6-424d-8c63-decda5f74b19', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('8eddb4f7-11a6-424d-8c63-decda5f74b19', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_axiom('8eddb4f7-11a6-424d-8c63-decda5f74b19', foundational, script_is_neutral_transcription_technology).
narrative_ontology:cs_axiom_status(script_is_neutral_transcription_technology, holdable).
narrative_ontology:cs_axiom_grounding('8eddb4f7-11a6-424d-8c63-decda5f74b19', script_is_neutral_transcription_technology, instrumental).
narrative_ontology:cs_axiom('8eddb4f7-11a6-424d-8c63-decda5f74b19', foundational, phonetic_fit_determines_script_adequacy).
narrative_ontology:cs_axiom_status(phonetic_fit_determines_script_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('8eddb4f7-11a6-424d-8c63-decda5f74b19', phonetic_fit_determines_script_adequacy, empirically_contingent).
narrative_ontology:cs_axiom('8eddb4f7-11a6-424d-8c63-decda5f74b19', secondary, transition_costs_are_one_time_amortizable).
narrative_ontology:cs_axiom_status(transition_costs_are_one_time_amortizable, holdable).
narrative_ontology:cs_axiom_grounding('8eddb4f7-11a6-424d-8c63-decda5f74b19', transition_costs_are_one_time_amortizable, empirically_contingent).
narrative_ontology:cs_reference_frame('8eddb4f7-11a6-424d-8c63-decda5f74b19', phonetically_optimal_neutral_standard).
narrative_ontology:cs_drift_state('8eddb4f7-11a6-424d-8c63-decda5f74b19', contemporary_revisionist_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8eddb4f7-11a6-424d-8c63-decda5f74b19', '').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, turkish_literate_public).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, republican_state_administration).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, transition_generation_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies and maintains the orthography: the 1928 Language Commission drafted the Latin-based alphabet under state direction, and its successor, the Turkish Language Association, publishes the orthography guide and standard dictionary, adjudicates spelling disputes, and defends the phonetic-fit rationale in public debate. It operates above the standard it administers; its own expertise and publications function in any script.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, turkish_language_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Enacted the Alphabet Law in 1928 and enforced the transition through schools, the adult Millet Mektepleri literacy campaign, and legal requirements that official printing use the new script. It gains a fully legible population, standardized administration and print, and a decision framed as technical rather than political, which shields it from revisionist challenge. Its own archives and operations are script-flexible.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, republican_state_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__phonetic_instrumentalism_reading, republican_state_administration, beneficiary).

% Reads and writes Turkish in the Latin orthography: all eight vowels are written distinctly, so any written word has one unambiguous pronunciation and literacy is acquired in months rather than years. Every text, sign, screen, and schoolbook they encounter uses it; their own literacy is Latin-script, so leaving the standard would mean acquiring a dead literacy at adult cost. They did not choose the standard and do not administer it; they use it and experience it as unqualified gain.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, turkish_literate_public, beneficiary,
    moderate, biographical, identity_locked, national).

% Adults literate in Ottoman script when the 1928 law took effect: their literacy became obsolete for public life overnight, and compliance meant night courses, literacy examinations for employment and civic standing, or functional illiteracy in the new public sphere. The reading prices their loss as a one-time relearning cost, amortized within a generation.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, transition_generation_learners, payer,
    powerless, biographical, trapped, national).

% Scribes, clerics, poets, and ordinary readers formed on the Ottoman abjad, for whom the script carried religious learning, literary tradition, and civilizational belonging. Their objection, that the reform severed the population from its own textual past and that script choice was never neutral, has no seat in a framing that treats writing systems as transcription instruments; in the technical account they appear only as bearers of transitional cost. Their competence persists in specialist and religious niches.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_script_literates, excluded,
    moderate, generational, identity_locked, national).

% Assesses the phonetic claims from outside Turkish politics: orthography typology confirms that an alphabet marking all eight vowels transcribes Turkish's vowel-harmonic phonology far more adequately than a three-vowel abjad, while the same literature treats script selection as historically entangled with state-building, religion, and geopolitics rather than determined by phonetic fit alone.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, comparative_linguistics_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__phonetic_instrumentalism_reading, diffuse).
narrative_ontology:fixing_cost_class(script_as_identity__phonetic_instrumentalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single standard orthography for Turkish: a phoneme-to-grapheme mapping in which all eight vowels are written distinctly, so any written word has one unambiguous pronunciation, literacy can be taught quickly, and print, administration, education, and now digital systems operate on one shared encoding.
% TRANSFER_FUNCTION: Moves transcription competence from a scribal elite trained in the three-vowel Ottoman abjad to the general population through a transparent alphabet; the transition itself moved the relearning cost onto the already-literate generation of 1928 while the durable gains accrued to every subsequent cohort of readers and to the state's administrative reach.
% ABSENT_VOICES: The Ottoman-script literate generation, clerics, scribes, poets, and their heirs, would object that script encodes civilizational identity, that the reform severed the population from its textual heritage, and that the phonetic argument was a rationalization; their objection is structurally absent from a framing in which writing systems are neutral instruments. Contemporary advocates of Ottoman-script religious education raise the same objection from outside the technical frame.
% DISAPPEARANCE_RATIONALE: If the Latin orthography requirement vanished overnight, every book, sign, screen, schoolbook, and official document in the country becomes illegible to its readers; education, administration, publishing, and digital infrastructure would have to re-encode or the population would have to relearn a predecessor script. The arrangement is the substrate of Turkish textual life, not an overlay on it.
% FOUNDING_PROBLEM: The Ottoman Arabic script is an abjad that marks only long vowels; applied to Turkish's eight-vowel, vowel-harmonic phonology it leaves short vowels unwritten, making written words systematically ambiguous in pronunciation and literacy expensive to acquire. The 1928 reform was built to solve that transcription problem with an alphabet that writes every vowel.
% FOUNDING_PROBLEM_CORROBORATION: Comparative orthography research outside the benefiting parties corroborates the technical core: Turkish's phonology is indeed transcribed far more adequately by a full-vowel alphabet than by the Arabic abjad. What no source outside the beneficiary set corroborates is the stronger neutrality claim that this technical fact made the choice apolitical; historians of the reform, including those sympathetic to it, document that civilizational orientation and identity motives were explicit at the time, so the neutrality framing itself stands uncorroborated.
narrative_ontology:disappearance_verdict(script_as_identity__phonetic_instrumentalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__phonetic_instrumentalism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__phonetic_instrumentalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__phonetic_instrumentalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__phonetic_instrumentalism_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__phonetic_instrumentalism_reading_tests).
:- end_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.12 at interval end) because the reading counts only the residual mandate cost above coordination benefit, and the transition costs it acknowledges amortize within a generation; the series declines 0.26 to 0.12 as those costs recede. Suppression is 0.18: no legal ban on Arabic-script Turkish remains, but the alternative is structurally closed, with no standard, no institutional support, and a population whose literacy is Latin-script; suppression is authored as this raw structural property and is not scaled by power or scope. The suppression_requirement series (0.78 to 0.18) traces enforcement decay, not liberalization of the arrangement's logic: the machinery, bans on Arabic-script publication, mandatory Millet Mektepleri courses, literacy examinations, built fast after 1928 and atrophied as identity lock made enforcement redundant. Theater rises 0.12 to 0.30: transcription activity is real and daily, but an increasing share of the arrangement's public life is commemorative and defensive, Harf Inkilabi anniversaries, textbook narratives, re-assertion against revisionist challenge, as living memory of the transition fades. Accessibility collapse is 0.60: the alternative is practically closed for ordinary purposes but persists in specialist and religious niches, as befits a convention rather than a natural law. Resistance (0.35) is episodic rather than sustained: the founding coercion met real opposition, and revisionist flare-ups, notably the Ottoman-Turkish curriculum controversies of the 2010s, recur without consolidating into a movement. All series share one time grid (1928, 1940, 1955, 1975, 1995, 2026) so no metric is sampled at a point where another metric's end-state would be substituted.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seats (state, TDK) the arrangement is a completed public good they built and administer: extraction invisible, theater invisible as theater. From the payer seat (transition generation) the same arrangement was overnight expropriation of their literacy, which the reading's own account prices at one generation's cost. The excluded seat (Ottoman-script literates) experiences the technical framing itself as the injury: a decision about civilizational belonging narrated as a decision about vowel notation. The literate public's seat is the reading's strongest evidence, near-uniform benefit, identity-locked into a standard experienced as pure gain, and simultaneously the reading's blind spot: the lock that reads as benefit from inside is what the continuity sibling reads as severed heritage. The depoliticization this reading performs is precisely the flattening of this perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (turkish_literate_public, republican_state_administration) derive low d: the orthography subsidizes its users with transparent transcription and the state with legibility. The victim declaration (transition_generation_learners) derives high d: the 1928 mandate extracted the existing value of their literacy. The identity_locked exit of the literate public would, read as captivity, push d toward the target end; the directionality override corrects this because the lock is constitutive of the benefit, their literacy IS the standard, and they experience no captivity. The state's dual seat (agenda_setter with beneficiary secondary) keeps its d low while recording that it both set the arrangement and collects from it. The excluded seat feeds no derivation; its absence is commentary-grade, not correction-grade.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading states, the Ottoman abjad's phonetic inadequacy for Turkish, is solved: no one proposes an abjad for Turkish on phonetic grounds, and founding_problem_status is authored dead while the arrangement persists with world_rearranges force. That mismatch, dead problem, rearranging world, diffuse gains, prohibitive fixing cost, is exactly the surface where mislabeling would occur: the receipt surface read alone (diffuse plus prohibitive) yields a piton-flavored verdict, a legacy standard held up by inertia and switching cost, while the reading's claim (rope) describes a live coordination good. The mandatrophy apparatus holds both without collapsing them: the phonetic mandate is complete (mandatrophy_resolved true), and what persists is either an inertial legacy standard drifting toward piton or a live identity-encoding institution that the technical framing obscures. The omega variables carry that unresolved alternative rather than letting either the rope claim or the piton-flavored receipt reading settle it by fiat. The classification thereby prevents the twin errors: mislabeling a genuine coordination standard as pure extraction, the continuity sibling's risk, and mislabeling an identity institution as neutral technology, this reading's own risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the phonetic_instrumentalism_reading of the script_as_identity kernel; what would the ottoman_continuity and kemalist_rupture sibling readings change about the structural classification of the same standing arrangement?',
    'Compare the sibling stories'' beneficiary/victim declarations and epsilon values: the kemalist_rupture_reading authors the Latin mandate as deliberate civilizational severance enabling secular modernization; the ottoman_continuity_reading authors the Arabic-script arrangement as identity-constitutive and the Latin mandate as amputation of the nation''s textual heritage, with a large victim set. The disagreement is located at one specific structural element: whether script is a neutral transcription technology or an identity-encoding institution.',
    'If the identity-encoding function is admitted into this reading''s structure, epsilon rises, the victim set expands beyond the transition generation, and the classification moves from rope toward tangled_rope; the neutrality claim is the precise element each sibling contests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate different constraints from the same 1928 arrangement.').

omega_variable(
    script_neutrality_claim,
    'Is the writing system a neutral transcription technology whose selection is fully determined by phonetic fit, or does every script choice encode civilizational identity and allegiance such that the neutrality framing is itself a political position?',
    'Orthography typology and comparative history: examine whether script reforms in structurally similar contexts (Central Asian latinization, Soviet cyrillization, other abjad-to-alphabet transitions) track phonetic adequacy alone or track geopolitical and civilizational alignment, and whether contemporaneous actors justified the 1928 choice in purely phonetic terms.',
    'If neutrality fails, this reading''s low epsilon is an artifact of its framing: heritage severance and coerced transition re-enter the structure and the classification shifts toward tangled_rope; if neutrality holds, the rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_neutrality_claim, conceptual, 'Whether the reading''s core premise (script neutrality) is true or is itself the depoliticizing move.').

omega_variable(
    phonetic_sufficiency_of_choice,
    'Did phonetic transparency actually determine the 1928 choice, or is the phonetic argument a post-hoc rationalization of a decision made on identity and geopolitical grounds, with the phonetic fit real but non-decisive?',
    'Archival study of the Language Commission deliberations and Ataturk''s private correspondence: were rival Latin schemes and a fully vowel-marked modified Arabic orthography (which some commission members proposed and which was technically available) evaluated and rejected on phonetic grounds, or before systematic phonetic analysis began?',
    'If the phonetic argument is post-hoc, this reading''s justification collapses into the kemalist_rupture sibling''s motive structure and this constraint''s independence from that sibling dissolves; if phonetic analysis genuinely drove the choice, the technical framing is accurate as far as it goes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonetic_sufficiency_of_choice, empirical, 'Whether the phonetic claim caused the reform or rationalizes it.').

omega_variable(
    persistence_mechanism_after_founding_problem,
    'The phonetic mismatch the reform solved no longer exists as a live problem; does the arrangement persist purely by inertia and prohibitive switching cost, or does it perform a live identity-encoding function that the technical framing obscures?',
    'Observe what arguments actually carry weight when script change is proposed: if resistance concentrates on civilizational and identity claims (as in the Ottoman-Turkish curriculum controversies of the 2010s) rather than switching-cost analysis, the live function is identity encoding; if cost-benefit alone explains the status quo, inertia is the whole story.',
    'If the live function is identity encoding, the arrangement is not an inertial legacy standard despite its dead founding problem and diffuse gains, and the rope claim misdescribes an active identity institution; if inertia suffices, the piton-flavored receipt surface is accurate and the rope claim describes a legacy coordination good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_mechanism_after_founding_problem, conceptual, 'Why the arrangement persists after its stated founding problem is solved.').

omega_variable(
    founding_coercion_visibility,
    'How much of the arrangement''s historical suppression (bans on Arabic-script publication, mandatory literacy exams, the coercive edges of the Millet Mektepleri campaign) belongs to the standing orthography constraint itself versus to the broader single-party state-building apparatus that happened to enforce it?',
    'Compare enforcement profiles across the contemporaneous Kemalist reforms (hat law, calendar reform, lexical purification): if script enforcement was distinctive in intensity or method, the suppression belongs to this constraint''s structure; if it was generic state coercion applied uniformly, the constraint''s own suppression is lower.',
    'Changes the interpretation of the suppression trajectory: current low suppression could reflect enforcement decay of a distinctively coercive constraint, or the completion of identity lock that made any constraint of this kind self-enforcing, a distinction the reading counts only one way.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_coercion_visibility, empirical, 'Attribution of the reform-era coercion between this constraint and its enforcement context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 1928, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1928, 0.12).
narrative_ontology:measurement(scri_tr_t1940, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(scri_tr_t1955, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1955, 0.18).
narrative_ontology:measurement(scri_tr_t1975, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(scri_tr_t1995, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1995, 0.26).
narrative_ontology:measurement(scri_tr_t2026, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(scri_be_t1928, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1928, 0.26).
narrative_ontology:measurement(scri_be_t1940, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1940, 0.2).
narrative_ontology:measurement(scri_be_t1955, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1955, 0.16).
narrative_ontology:measurement(scri_be_t1975, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement(scri_be_t1995, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1995, 0.13).
narrative_ontology:measurement(scri_be_t2026, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 2026, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1928, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1928, 0.78).
narrative_ontology:measurement(scri_su_t1940, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1940, 0.62).
narrative_ontology:measurement(scri_su_t1955, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1955, 0.45).
narrative_ontology:measurement(scri_su_t1975, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(scri_su_t1995, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1995, 0.22).
narrative_ontology:measurement(scri_su_t2026, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 2026, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, information_standard).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__ottoman_continuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Turkish script reform' decomposes into three structurally distinct constraints sharing one historical event, per the epsilon-invariance principle: this file (phonetic instrumentalism, low epsilon, technical optimization, beneficiaries = users and state, victims = transition generation only), script_as_identity__kemalist_rupture_reading (the mandate as civilizational rupture, different beneficiary structure and different epsilon), and script_as_identity__ottoman_continuity_reading (the Arabic-script arrangement as identity-constitutive, the Latin mandate as severance with a large victim set). The epsilon values differ because the readings assess the same standing arrangement by different lights; the files are linked by affects_constraints because each reading's justification is cited against the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__phonetic_instrumentalism_reading, moderate, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
