% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Created Quran Doctrine (Mutazilite Reading of Revelation's Ontological Status)
 *   domain: religious/political/intellectual
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the quran_ontological_status
 *   kernel: the Mutazilite doctrinal claim that the Quran is created divine
 *   speech and that God's essence transcends every temporal artifact,
 *   including revelation. The standing arrangement under contest, and the
 *   fixed referent of epsilon, is the doctrinal regime under which the
 *   community relates to revelation as a created artifact whose meaning is
 *   adjudicable by reason, assessed by this reading's own lights. The sibling
 *   readings (the uncreated-eternal-speech reading and the state-enforced
 *   creation reading with its inquisition) are separate constraints in
 *   separate files, not averaged into this one. The claim/metric gap is
 *   deliberate: the reading is CLAIMED as tangled_rope because it
 *   structurally couples a genuine coordination function (preserving divine
 *   unity) with asymmetric authority transfer (from jurists to theologians)
 *   under active institutional maintenance, while the authored metrics
 *   describe the constraint's end-state operation after its institutional
 *   defeat; the engine measures whatever divergence exists. Time points map
 *   approximately to 800-950 CE at three years per point; the inquisition of
 *   833-848 spans roughly T11-T16.
 *
 * KEY AGENTS:
 *   - mutazilite_rationalist_theologians: primary beneficiary (organized/identity_locked) — collects offices, stipends, and hermeneutic authority; runs the doctrinal program
 *   - abbasid_imperial_court: agenda setter (institutional/arbitrage) — proclaims, funds, and can reverse doctrinal policy
 *   - traditionalist_jurists: primary target (organized/identity_locked) — bears the expropriation of textual authority; resists at personal cost
 *   - literalist_devotional_communities: diffuse target (powerless/constrained) — bear the cost of mediated devotion; coalition-capable through numbers
 *   - falsifa_philosophical_schools: secondary beneficiary (moderate/mobile) — flourishes in the space the doctrine opens without owing it loyalty
 *   - historians_of_kalam: analytical observer — sees the full structure from outside all commitments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.36).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.26).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Created Quran Doctrine (Mutazilite Reading of Revelation's Ontological Status)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "religious/political/intellectual").

domain_priors:requires_active_enforcement(quran_ontological_status__created_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '2824108e-708d-402a-962f-1debee2d5fdc').
narrative_ontology:cs_kernel_codification('2824108e-708d-402a-962f-1debee2d5fdc', distributed).
narrative_ontology:cs_authority_grounding('2824108e-708d-402a-962f-1debee2d5fdc', expertise).
narrative_ontology:cs_interpretation_layer_present('2824108e-708d-402a-962f-1debee2d5fdc').
narrative_ontology:cs_reading_relation('2824108e-708d-402a-962f-1debee2d5fdc', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('2824108e-708d-402a-962f-1debee2d5fdc', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('2824108e-708d-402a-962f-1debee2d5fdc', foundational, divine_unity_excludes_coeternal_speech).
narrative_ontology:cs_axiom_status(divine_unity_excludes_coeternal_speech, holdable).
narrative_ontology:cs_axiom_grounding('2824108e-708d-402a-962f-1debee2d5fdc', divine_unity_excludes_coeternal_speech, theological).
narrative_ontology:cs_axiom('2824108e-708d-402a-962f-1debee2d5fdc', secondary, reason_adjudicates_scriptural_meaning).
narrative_ontology:cs_axiom_status(reason_adjudicates_scriptural_meaning, holdable).
narrative_ontology:cs_axiom_grounding('2824108e-708d-402a-962f-1debee2d5fdc', reason_adjudicates_scriptural_meaning, instrumental).
narrative_ontology:cs_reference_frame('2824108e-708d-402a-962f-1debee2d5fdc', absolute_divine_transcendence_framework).
narrative_ontology:cs_drift_state('2824108e-708d-402a-962f-1debee2d5fdc', post_mihna_traditionist_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('2824108e-708d-402a-962f-1debee2d5fdc', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, mutazilite_rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, falsifa_philosophical_schools).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_devotional_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff the judiciary and teaching circles of Baghdad and Basra; argue in caliphal disputations that God's oneness requires His spoken word, as an artifact in Arabic sounds and inscriptions, to be a created thing rather than a second eternal beside Him. Collect stipends, judgeships, and the standing that follows court favor. Their school's entire method stands or falls with the doctrine, so abandoning it would mean dismantling their own lifework.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, mutazilite_rationalist_theologians, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, mutazilite_rationalist_theologians, agenda_setter).

% Sets religious policy through appointments of judges, market inspectors, and endowed teaching posts; proclaimed the created-Quran affirmation as a condition of office and funded its defenders. Gains doctrinal uniformity and a religious class dependent on imperial favor. Can reverse course when political winds shift, as it ultimately did under al-Mutawakkil.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, abbasid_imperial_court, agenda_setter,
    institutional, generational, arbitrage, continental).

% Hadith scholars and jurists whose interpretive authority rests on transmitting the Prophet's words and treating the revealed text as God's own eternal speech. Refuse the created-doctrine affirmation even at the cost of imprisonment and flogging, as Ahmad ibn Hanbal did. Command deep popular loyalty that outlasts court patronage. Accepting the doctrine would surrender the foundation on which their authority and livelihood rest.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    organized, biographical, identity_locked, continental).

% Ordinary believers and popular preachers whose prayer and recitation treat the heard verses as God speaking directly. Under the doctrine their practice becomes an act requiring expert mediation: the recited words are a created expression, and what they mean is settled by credentialed theologians. Leaving would mean abandoning the devotional world they inhabit; their strength is mass numbers and sympathy for the persecuted traditionists, but they lack elite access.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_devotional_communities, payer,
    powerless, generational, constrained, regional).

% Philosophers writing in Arabic, such as al-Kindi and his successors, gain a public culture in which reason may investigate even sacred text. The doctrine's victory over literalism keeps the door open for demonstrative science and allegorical reading. They owe no loyalty to the specific doctrine yet flourish in the space it opens, and can move between courts and patrons if conditions change.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, falsifa_philosophical_schools, beneficiary,
    moderate, generational, mobile, continental).

% Later chroniclers and modern scholars reconstruct the dispute from heresiographies, inquisition records, and surviving treatises. They see the full structure of doctrine, patronage, coercion, and resistance from outside any party's commitments, and can compare communities that held the doctrine with and without state backing.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, historians_of_kalam, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, mutazilite_rationalist_theologians).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves strict monotheism by locating eternity solely in God's essence: if the Quran were coeternal with God, something other than God would be eternal, and the unity confession would fracture. The doctrine also lets the community engage scripture as a temporal artifact open to rational investigation, coordinating religious instruction around demonstrable theology rather than transmitted report alone.
% TRANSFER_FUNCTION: Moves interpretive authority, together with the offices, stipends, and prestige attached to it, from traditionist jurists and literalist communities to rationalist theologians credentialed in dialectical theology; moves doctrinal allegiance itself from transmitted report toward rational demonstration.
% ABSENT_VOICES: Traditionist jurists opposed the doctrine publicly but were excluded from the court councils where doctrine was set, and during the inquisition its partisans were literally imprisoned. Ordinary believing communities had no seat at all: the dispute was conducted among elites while the devotional practice of millions was the object being redefined.
% DISAPPEARANCE_RATIONALE: If the created-doctrine arrangement vanished overnight, the authority map of Abbasid religious life would reorganize: jurists would regain unmediated textual authority, rationalist theology would lose its institutional footing, judicial and teaching appointments would change hands, and the boundary of acceptable belief would redraw around the uncreated reading. Something close to this actually happened when imperial patronage withdrew.
% FOUNDING_PROBLEM: The crisis of divine unity and anthropomorphism: scriptural descriptions of God's hand, face, and speech threatened to make God corporeal or composite, and an eternal Quran would place a second eternal beside God. The doctrine was built to solve how revelation's authority could be affirmed while God remained absolutely one and transcendent.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists for the problem from outside the benefiting parties: Christian mutakallimun of the same era debated identical unity-and-anthropomorphism puzzles, Jewish mutakallimun later adopted the rationalist method wholesale across a confessional line, and the doctrine's eventual Ash'ari opponents conceded the force of the unity concern while rejecting the createdness solution. Whether the problem remains live, and whether this solution was ever needed, is disputed between the surviving schools.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).
:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.36 at interval end) is real but bounded: the doctrine's characteristic yield was interpretive authority and the offices attached to it, not wealth, and after the imperial reversal the yield contracted to the communities still holding the position. Suppression (0.26 end-state) is authored as a raw structural property, unscaled by power or scope; the series traces enforcement machinery building toward the inquisition era and decaying afterward. Attribution discipline matters here: the inquisition's coercive excess belongs to the sibling state_enforced_creation_reading, so this story's suppression series crests at the buildup's edge (0.58 at T16) rather than at the inquisition's actual intensity — the boundary is documented in the doctrine_enforcement_separability omega. Theater peaks with courtly disputation as spectacle and falls as the patronage stage disappears. Accessibility collapse is low (0.35): alternatives never closed, the uncreated reading survived persecution underground and in popular sympathy, and ultimately won. Resistance is high (0.72): the traditionist stand, ibn Hanbal's trial, and mass sympathy are among the best-documented resistances in medieval intellectual history. All three tracked series share one time grid (T0, 8, 16, 24, 32, 42, 50) so no metric row borrows another's end-state; base_properties scalars match the T50 column by design, describing the constraint's residual operation at interval end.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the theologian seat the arrangement is the rescue of monotheism itself: without it, God shares eternity with a book. From the jurist seat the same structure is expropriation: authority over meaning, and the livelihood attached to it, transferred to rivals credentialed in a foreign method. From the court seat it is an instrument of administrative uniformity, valuable exactly as long as it delivers compliance and disposable when it costs legitimacy. From the believer's seat the ontology is nearly invisible; what registers is that the words recited in prayer now require an expert to say what they are. The engine derives these divergent classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The theologian seat sits near the beneficiary pole: the doctrine subsidizes its holders with office and standing, and their identity lock amplifies attachment rather than exposure. The jurist seat sits near the target pole: identity-locked exit means the authority taken from them cannot be relocated, so the extraction lands at close to full weight despite their organized power. Literalist communities are targets with weaker positional atoms but coalition potential — their mass sympathy for the persecuted traditionists is precisely the coalition channel the analysis flags for powerless agents. The court is structurally mixed: it sets the agenda and collects legitimation, but it also pays when reversal is needed, which is why its derived directionality sits nearer the middle than a pure beneficiary's. The philosophers are incidental beneficiaries with mobile exit, damping their effective exposure toward the subsidized end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unity versus anthropomorphism) is contested rather than dead: the unity concern remains live in every surviving school, while the specific mandate — making createdness the enforced creed — died with the political reversal. Classifying the arrangement as tangled_rope prevents two opposite mislabels. Calling it a mountain would assert that revelation's created status is a structural feature of reality; the record shows a constructed, contested, ultimately defeated position, the opposite profile. Calling it a snare would erase the genuine coordination function: the doctrine really did solve a unity problem that cross-confessional corroboration shows was felt outside the benefiting parties, and its suppression never approached the closure needed for a pure trap. The tangled_rope reading keeps both halves visible: coordination worth having, extraction worth naming, enforcement doing the holding. The receipt surface records where the yield went (the theologian seat) and that fixing was cheap for the seat that could fix it (the court reversed it within a decade of willing to), which is exactly the signature of a doctrine sustained by patronage rather than by necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_enforcement_separability,
    'Is the pure created-doctrine arrangement separable from its state-enforcement variant, or does the doctrine''s historical operation include the inquisition''s coercion?',
    'Compare extraction and suppression profiles across communities holding the doctrine with and without state power: Zaydi enclaves, Jewish mutakallimun, and post-imperial Mutazili circles versus the Abbasid court establishment.',
    'If inseparable, this story understates the combined regime''s suppression and the sibling boundary should shift; if separable, the doctrinal constraint''s own profile stands as authored and the inquisition''s excess is wholly the sibling''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_enforcement_separability, empirical, 'Boundary between the doctrinal reading and its state-enforced sibling').

omega_variable(
    kernel_sibling_structural_delta,
    'This constraint is one reading of the quran_ontological_status kernel; what structurally changes if the uncreated sibling is adopted instead?',
    'Author and compile the uncreated_reading story and compare computed per-seat classifications: the prediction is that beneficiary and victim sets invert, with fixity-policing becoming the authority-conferring activity.',
    'Under the sibling, revelation functions as a fixed ontic fixture rather than a flexible artifact, rationalist hermeneutic authority collapses, and the seats currently computed as targets become the agenda-holders; every directional value in this story flips sign.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_structural_delta, conceptual, 'Committer-frame delta against the uncreated sibling reading').

omega_variable(
    sincerity_composition,
    'What share of the doctrine''s institutional persistence was sincere conviction solving the unity problem versus patronage-seeking dressed as theology?',
    'Prosopography of adherents'' careers: did doctrinal adherence precede or follow reward, and did adherents maintain the position after patronage withdrew?',
    'A high patronage-share pushes the reading toward the pure-extraction end and strengthens the snare-side computation; a high sincerity share supports the coordination-first reading and dampens effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_composition, empirical, 'Conviction versus rent composition of the doctrine''s support base').

omega_variable(
    jurist_authority_refoundation,
    'Could traditionalist jurists have re-founded their authority on bases independent of the text''s ontology, lowering the extraction they effectively bear?',
    'Examine jurists who partially did so: the development of legal methodology (usul al-fiqh) decoupling procedural authority from ontological claims about the text.',
    'If refoundation was viable, the jurist seat''s effective extraction drops and the arrangement reads closer to a coordination mechanism with friction; if the ontology was load-bearing for their authority, the target-side computation stands at full weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurist_authority_refoundation, conceptual, 'Whether the victim seat''s identity lock was escapable in principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t8, quran_ontological_status__created_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(qura_tr_t16, quran_ontological_status__created_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(qura_tr_t24, quran_ontological_status__created_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(qura_tr_t32, quran_ontological_status__created_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(qura_tr_t42, quran_ontological_status__created_reading, theater_ratio, 42, 0.18).
narrative_ontology:measurement(qura_tr_t50, quran_ontological_status__created_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(qura_be_t8, quran_ontological_status__created_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(qura_be_t16, quran_ontological_status__created_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(qura_be_t24, quran_ontological_status__created_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(qura_be_t32, quran_ontological_status__created_reading, base_extractiveness, 32, 0.46).
narrative_ontology:measurement(qura_be_t42, quran_ontological_status__created_reading, base_extractiveness, 42, 0.4).
narrative_ontology:measurement(qura_be_t50, quran_ontological_status__created_reading, base_extractiveness, 50, 0.36).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(qura_su_t8, quran_ontological_status__created_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(qura_su_t16, quran_ontological_status__created_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(qura_su_t24, quran_ontological_status__created_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(qura_su_t32, quran_ontological_status__created_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(qura_su_t42, quran_ontological_status__created_reading, suppression_requirement, 42, 0.32).
narrative_ontology:measurement(qura_su_t50, quran_ontological_status__created_reading, suppression_requirement, 50, 0.26).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Mutazilite doctrine of the created Quran' decomposes into two structurally distinct stories per the epsilon-invariance principle. This story authors the doctrinal claim itself — authority transfer through hermeneutic restructuring, moderate extraction, enforcement through patronage and appointment. The sibling state_enforced_creation_reading authors the same doctrine plus the imperial inquisition — the same coordination function with coercive suppression layered on, substantially higher epsilon and suppression. The doctrinal story upstream-supplies the warrant the enforcement story executes; the enforcement story retroactively discredited the doctrine by association, accelerating its defeat. The uncreated_reading is the third family member and this reading's direct contradictory. All three are linked via network.affects_constraints; measuring the family through either member alone misattributes the other's coercion or concedes the rival's ontology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
