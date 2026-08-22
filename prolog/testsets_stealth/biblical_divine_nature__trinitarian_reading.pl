% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__trinitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__trinitarian_reading
 *   human_readable: Nicene Trinitarian Settlement: Essence-Unity Reading of the Divine Nature
 *   domain: theology/religious authority/doctrinal history
 *
 * SUMMARY:
 *   The Nicene settlement is the institutional arrangement that fixed the
 *   grammar of Christian God-talk: three hypostases, one ousia, monotheism
 *   preserved through essence-unity. It solved a genuine fourth-century
 *   coordination problem — how to confess one God while continuing the
 *   worship the churches already offered to Christ — and it did so through
 *   councils backed, from Theodosius onward, by state power. The same
 *   structure that unified the church consolidated episcopal authority and
 *   criminalized rival readings: Arian and subordinationist communities were
 *   exiled and dispossessed in the ancient period, and anti-Trinitarians
 *   faced execution as late as Servetus in 1553. Today enforcement is soft —
 *   ordination gates, ecumenical exclusion, social cost — but the boundary
 *   machinery remains active. This story is ONE reading of the contested
 *   kernel biblical_divine_nature; the unitarian and modalist readings are
 *   separate constraints with their own files, linked through the network
 *   section.
 *
 * KEY AGENTS:
 *   - - trinitarian_episcopal_hierarchy: Agenda-setter (institutional/identity_locked) — defines and administers the formula; its authority is constituted by the settlement it guards
 *   - - imperial_and_state_authorities: Conditional beneficiary and historical co-enforcer (institutional/mobile) — exchanged police power for legitimation; has largely exited
 *   - - orthodox_christian_communities: Beneficiary (organized/constrained) — receives shared worship grammar and mutual recognition
 *   - - lay_worshipping_majority: Dual-positioned beneficiary/payer (organized/constrained) — receives liturgical identity, pays unexamined assent
 *   - - anti_nicene_minority_sects: Historical target (powerless/trapped) — exiled, dispossessed, suppressed under imperial law
 *   - - modern_non_trinitarian_denominations: Contemporary target (organized/constrained) — legally free, institutionally gated
 *   - - doctrinal_historians: Analytical observer (analytical/analytical) — sees the full structure including the suppression record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.42).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.32).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Nicene Trinitarian Settlement: Essence-Unity Reading of the Divine Nature").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theology/religious authority/doctrinal history").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, 'a434cb03-d023-485b-ae75-b47fba372fde').
narrative_ontology:cs_kernel_codification('a434cb03-d023-485b-ae75-b47fba372fde', formalized).
narrative_ontology:cs_authority_grounding('a434cb03-d023-485b-ae75-b47fba372fde', lineage).
narrative_ontology:cs_interpretation_layer_present('a434cb03-d023-485b-ae75-b47fba372fde').
narrative_ontology:cs_reading_relation('a434cb03-d023-485b-ae75-b47fba372fde', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('a434cb03-d023-485b-ae75-b47fba372fde', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('a434cb03-d023-485b-ae75-b47fba372fde', foundational, homoousion_consubstantiality).
narrative_ontology:cs_axiom_status(homoousion_consubstantiality, holdable).
narrative_ontology:cs_axiom_grounding('a434cb03-d023-485b-ae75-b47fba372fde', homoousion_consubstantiality, theological).
narrative_ontology:cs_axiom('a434cb03-d023-485b-ae75-b47fba372fde', secondary, creedal_boundary_enforcement_legitimate).
narrative_ontology:cs_axiom_status(creedal_boundary_enforcement_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('a434cb03-d023-485b-ae75-b47fba372fde', creedal_boundary_enforcement_legitimate, conventional).
narrative_ontology:cs_reference_frame('a434cb03-d023-485b-ae75-b47fba372fde', nicene_constantinopolitan_settlement).
narrative_ontology:cs_drift_state('a434cb03-d023-485b-ae75-b47fba372fde', contemporary_ecumenical_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a434cb03-d023-485b-ae75-b47fba372fde', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, trinitarian_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, orthodox_christian_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, anti_nicene_minority_sects).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, modern_non_trinitarian_denominations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, imperial_and_state_authorities).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, lay_worshipping_majority).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, lay_worshipping_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Councils, bishops, and their theological faculties define the credal formula, convene synods, and administer ordination and communion around it. The formula's stability is the ground of the teaching office itself: revising it would unmake the authority that guards it, so departure from the settlement is not a live option for the office. It collects deference, jurisdiction, and — in the coercive phases of the interval — state-backed enforcement powers.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, trinitarian_episcopal_hierarchy, agenda_setter,
    institutional, generational, identity_locked, global).

% From Theodosius onward, empires and kingdoms lent police power to the doctrinal boundary in exchange for legitimation; later confessional states ran heresy courts and inquisitions. Their gain was legitimation and social cohesion; their cost was the enforcement apparatus itself. Modern secular states have largely withdrawn from enforcement entirely, keeping neutrality — demonstrating that their position in the arrangement was always contingent and reversible.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, imperial_and_state_authorities, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, imperial_and_state_authorities, agenda_setter).

% Congregations and communions in the Nicene inheritance receive a shared worship grammar, clear membership boundaries, and mutual recognition across languages, cultures, and centuries. They bear the ongoing catechetical and disciplinary costs of maintaining the boundary and cannot easily abandon it without losing recognition from the wider communion.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, orthodox_christian_communities, beneficiary,
    organized, generational, constrained, global).

% Ordinary believers receive ready-made answers to who God is, a stable liturgical identity, and membership in a community spanning millennia. They pay conformity: reciting formulas most never study closely, and accepting the exclusion of neighbors who read the same scriptures differently. Leaving costs them their religious community; staying costs them assent they may not personally hold.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, lay_worshipping_majority, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, lay_worshipping_majority, payer).

% Fourth-century and medieval communities that read the Son as subordinate or created — Arians, Eunomians, Homoians — lost offices, property, and under imperial law their physical security; bishops were exiled, assemblies banned, books burned. Their options were conformity or disappearance. As a class they lacked the coalition weight to resist once the imperial alliance formed.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, anti_nicene_minority_sects, payer,
    powerless, generational, trapped, regional).

% Unitarians, Oneness Pentecostals, Christadelphians, and Jehovah's Witnesses are legally free today but gated out of ordination recognition, ecumenical bodies, and mainstream Christian respectability. Their millions of adherents grow at the boundary rather than inside it, bearing persistent social and institutional costs of dissent while lacking any seat in the bodies that maintain the formula.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, modern_non_trinitarian_denominations, payer,
    organized, generational, constrained, global).

% Scholars of patristics and doctrinal history reconstruct how the formula was reached, what was suppressed along the way, and how enforcement rose and fell. They hold no seat in the arrangement, collect nothing from it, and can therefore say things about its operation that participants inside it could not.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, doctrinal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__trinitarian_reading, trinitarian_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(biblical_divine_nature__trinitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one grammatical rule for Christian speech about God — distinct Father, Son, and Spirit sharing one divine essence — so that congregations across languages and centuries can baptize, sing, and confess together without collapsing into either three gods or one flat person acting in sequence. It settled a live fourth-century dispute about whether the Son is truly divine by finding a formula that preserved both monotheism and the worship practice the churches already directed at Christ.
% TRANSFER_FUNCTION: Moves doctrinal assent and institutional loyalty from every baptized Christian toward the council-and-bishop office that certifies orthodoxy; in the coercive phases of the interval it also moved offices, property, and physical security away from dissenting teachers toward conforming ones.
% ABSENT_VOICES: Anti-Nicene teachers were condemned and barred from the very councils that fixed the wording — Arius was condemned before the formula reached its final form, and no Arian seat shaped the settlement that anathematized him. Modern non-Trinitarians sit outside the ecumenical bodies whose membership criteria presume the answer. The apparent unanimity of the settlement is partly an artifact of who was permitted in the room when the wording was fixed.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, Christian institutions would lose their primary boundary marker: ordination standards, ecumenical recognition, catechetical curricula, and most historic liturgies presuppose the formula. Worship of God would certainly continue, but communities would rearrange along the sibling lines — subordinationist, modalist, and essence-unity congregations — and fourteen centuries of accumulated institutional recognition would need renegotiating.
% FOUNDING_PROBLEM: After Arius taught that the Son was a created intermediary, the church faced a choice between the strict monotheism it confessed and the worship it already offered to the risen Christ and experienced in the Spirit. The arrangement was built to hold both in a single formulable sentence: one God, and a truly divine Son and Spirit, without arithmetic contradiction.
% FOUNDING_PROBLEM_CORROBORATION: Jewish and Muslim theologians have kept the underlying tension alive for fourteen centuries by pressing the arithmetic of one-plus-one-plus-one against the formula — their objection is the founding problem restated from outside. Secular historians of doctrine from Gibbon through Harnack to contemporary patristics document the dispute's reality independent of any church interest. Analytic philosophers of religion still publish actively on whether the essence-person distinction is coherent. None of these corroborating seats benefits from the arrangement.
narrative_ontology:disappearance_verdict(biblical_divine_nature__trinitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__trinitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__trinitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__trinitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__trinitarian_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__trinitarian_reading_tests).
:- end_tests(biblical_divine_nature__trinitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42 at interval end) reflects the contemporary arrangement: the formula is decoupled from any service the payer seats could refuse — dissenting readings of the same scriptures carry no additional administrative cost to gate out, yet gating them is what the boundary machinery does. The temporal series shows the honest arc: extraction and enforcement climbed together from Nicaea through the confessional-state peak (0.66 at 1553, when the machinery was killing people), then decayed as church-state fusion dissolved. Suppression (0.32 scalar) is authored as a raw structural property — unscaled by power or scope — describing today's institutional gatekeeping; the suppression_requirement series separately traces the enforcement-capacity ratchet and decay, which is the dynamic this story tracks. Theater (0.32) is real but modest: creedal recitation increasingly outruns comprehension, yet the formula still functions in liturgy, catechesis, and ecumenical recognition. Accessibility_collapse (0.44) is moderate because alternatives visibly persist — Unitarian and Oneness bodies number in the millions — while collapsing almost completely inside confessional institutions. Resistance (0.58) has been continuous from Arius to the present. Claim and metrics are independent authored facts: I claim tangled_rope because the structure possesses BOTH a genuine coordination function (the worship-grammar problem was real and the formula solved it) AND asymmetric extraction with named victims and active enforcement; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the episcopal seat the arrangement is guardianship: the hierarchy experiences subsidy (meaning, jurisdiction, continuity) and would classify the settlement as the price of coherent monotheistic worship. From the dissenting seats — historical and modern — the same structure operates as enforced exclusion: identical scriptures, different conclusion, locked outside the recognition economy. The lay majority sits near symmetric, receiving liturgical identity while paying unexamined assent. The state seat is the clearest demonstration that position, not doctrine, determined experience: the same authorities that enforced the formula for sixteen centuries withdrew within two generations once the legitimation exchange stopped paying.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the hierarchy sits nearest the beneficiary end (d low) — it collects deference and jurisdiction and bears only the cost of its own conviction; orthodox communities sit low-to-symmetric (genuine coordination benefit, real maintenance cost); the lay majority is near symmetric by its dual beneficiary/payer position. Victim declarations drive the targets upward: anti-Nicene sects sat near the full-target end (d high) with trapped exit — conformity or disappearance; modern non-Trinitarian denominations remain high-d with constrained exit (free to exist, not free to be recognized). The state seat is the one place the plain derivation misleads: declared a beneficiary, it also paid the enforcement bill and eventually walked away, so its effective position is more symmetric than its beneficiary role suggests — I note this here rather than overriding, because the override surface is keyed by power atom and would distort the hierarchy seat sharing that atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is still live: the tension between numerical monotheism and the worship of a divine Christ regenerates wherever Christianity meets strict monotheists or critical readers, and the analytic philosophy of religion literature is evidence the conceptual work is unfinished. Because the founding problem is live, this is NOT a mandatrophy case — the arrangement has not outlived its function, and declaring mandatrophy_resolved would be false. The classification discipline cuts both ways here: reading the arrangement as pure extraction ignores the real coordination function the formula performed and performs (the sibling readings are attempts to solve the same problem, not refutations that no problem existed); reading it as pure coordination ignores the named victims, the anathema record, and the fact that the winning formula was fixed by councils from which the losers were barred. The tangled_rope claim holds both facts without letting either erase the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the trinitarian_reading of kernel biblical_divine_nature — the disagreement with the sibling readings is located in the ontological status of the Son and Spirit relative to the Father''s deity. How would the sibling readings (unitarian_reading, modalist_reading) restructure the arrangement''s beneficiary and victim sets?',
    'Comparative read of the three sibling story files: each names its own beneficiaries, victims, and enforcement structure; the differences locate the structural disagreement precisely.',
    'Under the unitarian reading the enforcement target set inverts — the essence-unity majority becomes the deviant party and the subordinationist reading claims the martyr lineage. Under the modalist reading the simultaneous-three-persons grammar itself becomes the error and the victim set shifts to those insisting on real distinctions of persons. Every classification in THIS file presumes THIS reading''s victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading among three of the divine-nature kernel; sibling readings are separate constraints.').

omega_variable(
    essence_person_coherence,
    'Is the hypostasis/ousia distinction coherent metaphysics, or an authority-shielded placeholder whose intelligibility is maintained by institutional enforcement rather than argument?',
    'Progress in analytic theology: social trinitarianism, relative-identity models, and constitutional accounts either render the formula rigorously statable or expose it as equivocation between senses of ''person'' and ''essence''.',
    'If the distinction proves coherent, the coordination function is genuine and the measured extraction is the historical cost of boundary maintenance. If it proves equivocal, the formula''s coordination value collapses toward boundary-marking alone, the extraction share rises, and the arrangement drifts toward the enforcement-only profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essence_person_coherence, conceptual, 'Whether the core metaphysical distinction is load-bearing or shielded.').

omega_variable(
    enforcement_revival_contingency,
    'Is the decay of coercive enforcement a permanent feature of the arrangement, or contingent on the current church-state settlement such that fusion conditions would restore the anathema machinery?',
    'Comparative analysis of jurisdictions where religious law regains state enforcement power: whether doctrinal boundary enforcement revives there tracks the contingency hypothesis.',
    'If contingent, the endpoint extraction and suppression values describe a favorable equilibrium, not a stable property, and the interval-end classification carries hidden variance. If permanent, the decay is structural and the arrangement has irreversibly shifted toward soft gatekeeping.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_revival_contingency, empirical, 'Whether enforcement decay is structural or regime-contingent.').

omega_variable(
    formula_necessity_vs_siblings,
    'Is the homoousion formula necessary for coherent Christian monotheistic worship, or one of several viable solutions — the unitarian and modalist siblings being live competitors rather than failed ones?',
    'Liturgical and theological analysis of whether the sibling formulas sustain equivalent worship grammar, baptismal coherence, and cross-cultural recognizability without the costs this arrangement''s victims paid.',
    'If the siblings are viable, part of what the enforcement machinery defended was a selection among peers rather than truth against error, and the extraction record weighs more heavily. If the formula is uniquely adequate, the enforcement history remains condemnable but the coordination function is vindicated as irreplaceable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formula_necessity_vs_siblings, conceptual, 'Whether the winning formula was necessary or merely victorious.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__trinitarian_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement(bibl_tr_t381, biblical_divine_nature__trinitarian_reading, theater_ratio, 381, 0.12).
narrative_ontology:measurement(bibl_tr_t800, biblical_divine_nature__trinitarian_reading, theater_ratio, 800, 0.18).
narrative_ontology:measurement(bibl_tr_t1553, biblical_divine_nature__trinitarian_reading, theater_ratio, 1553, 0.22).
narrative_ontology:measurement(bibl_tr_t1780, biblical_divine_nature__trinitarian_reading, theater_ratio, 1780, 0.26).
narrative_ontology:measurement(bibl_tr_t1900, biblical_divine_nature__trinitarian_reading, theater_ratio, 1900, 0.28).
narrative_ontology:measurement(bibl_tr_t2025, biblical_divine_nature__trinitarian_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__trinitarian_reading, base_extractiveness, 325, 0.3).
narrative_ontology:measurement(bibl_be_t381, biblical_divine_nature__trinitarian_reading, base_extractiveness, 381, 0.42).
narrative_ontology:measurement(bibl_be_t800, biblical_divine_nature__trinitarian_reading, base_extractiveness, 800, 0.52).
narrative_ontology:measurement(bibl_be_t1553, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1553, 0.66).
narrative_ontology:measurement(bibl_be_t1780, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1780, 0.56).
narrative_ontology:measurement(bibl_be_t1900, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1900, 0.47).
narrative_ontology:measurement(bibl_be_t2025, biblical_divine_nature__trinitarian_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__trinitarian_reading, suppression_requirement, 325, 0.35).
narrative_ontology:measurement(bibl_su_t381, biblical_divine_nature__trinitarian_reading, suppression_requirement, 381, 0.5).
narrative_ontology:measurement(bibl_su_t800, biblical_divine_nature__trinitarian_reading, suppression_requirement, 800, 0.6).
narrative_ontology:measurement(bibl_su_t1553, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1553, 0.72).
narrative_ontology:measurement(bibl_su_t1780, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1780, 0.55).
narrative_ontology:measurement(bibl_su_t1900, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1900, 0.38).
narrative_ontology:measurement(bibl_su_t2025, biblical_divine_nature__trinitarian_reading, suppression_requirement, 2025, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, modalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the doctrine of the Trinity' conflates three structurally distinct arrangements that are competing READINGS of one kernel (biblical_divine_nature). Each reading instantiates a different constraint with its own epsilon, beneficiary/victim structure, and enforcement profile: this file (trinitarian_reading — essence-unity, institutional enforcement, dissenters as victims), unitarian_reading (numerical singularity; historically the victim set before Nicaea's victory, today the gated-out set), and modalist_reading (sequential modes; condemned as Sabellianism, surviving in Oneness movements). The upstream settlement influences the downstream siblings' operating environment — its anathemas defined them as heresies and its recognition economy prices their dissent — which is why this story links to both. Per the epsilon-invariance principle, no single story averages across the readings; the contest lives in the comparison, not inside any one file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
