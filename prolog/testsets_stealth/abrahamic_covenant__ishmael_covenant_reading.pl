% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__ishmael_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__ishmael_covenant_reading, []).

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
 *   constraint_id: abrahamic_covenant__ishmael_covenant_reading
 *   human_readable: Ishmael-Covenant Reading: Inclusive Abrahamic Transmission through Muhammad
 *   domain: religious/institutional-authority
 *
 * SUMMARY:
 *   The Abrahamic covenant — the Genesis promise to Abraham of blessing,
 *   multitude, and inheritance — is a single persisting commitment that rival
 *   communities read differently; this file instantiates exactly one reading:
 *   that the covenant continues through Ishmael, validated by the prophetic
 *   succession culminating in Muhammad, with Genesis's promises to Ishmael
 *   (Genesis 17:20) read as operative covenant terms rather than incidental
 *   blessings. As an operative arrangement the reading coordinates a
 *   planetary community around a shared Abrahamic identity while imposing
 *   real costs: rival heirs' narrations are demoted to error, post-Quranic
 *   prophetic claims are defined as heresy, and historically non-Muslim
 *   subjects lived under contractual subordination the covenant hierarchy
 *   helped legitimate. Per the epsilon-invariance principle this file does
 *   not average over the kernel's other readings — the Isaac-exclusive
 *   reading, the Christian supersessionist reading, and the territorial
 *   land-promise reading are separate constraints (linked below) with their
 *   own epsilon, beneficiaries, and victims. The epsilon referent here is the
 *   standing Ishmael-covenant arrangement itself, assessed by the reading's
 *   own lights: generous admission, real boundary enforcement. Claim and
 *   metrics are authored independently: the reading is CLAIMED as
 *   tangled_rope (genuine coordination plus asymmetric extraction through the
 *   same structure) and the metrics describe moderately extractive, variably
 *   enforced operation. KEY AGENTS (by structural relationship): -
 *   global_muslim_ummah: primary beneficiary (organized/identity_locked) —
 *   receives covenant identity, bears compliance burdens -
 *   islamic_scholarly_establishment: agenda_setter
 *   (institutional/constrained) — administers the reading, collects custodial
 *   standing - arab_descendant_communities: secondary beneficiary
 *   (moderate/mobile) — descent prestige, low binding cost -
 *   converts_to_islam: inclusion-side beneficiary (moderate/identity_locked)
 *   — frictionless entry, heavy rear door - rival_abrahamic_heirs: primary
 *   target (organized/constrained) — rival deed demoted to error -
 *   post_quranic_prophetic_claimants: acute target (powerless/trapped) —
 *   defined as heresy by the finality clause - comparative_religion_scholars:
 *   analytical observer — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.52).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.6).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Ishmael-Covenant Reading: Inclusive Abrahamic Transmission through Muhammad").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious/institutional-authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, '0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0').
narrative_ontology:cs_kernel_codification('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', fixed_text).
narrative_ontology:cs_authority_grounding('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', lineage).
narrative_ontology:cs_interpretation_layer_present('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0').
narrative_ontology:cs_reading_relation('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', abrahamic_covenant__isaac_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', abrahamic_covenant__christian_supersessionist_reading, forecloses).
narrative_ontology:cs_reading_relation('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', abrahamic_covenant__land_promise_constraint, influences).
narrative_ontology:cs_axiom('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', foundational, covenant_extends_through_ishmael_line).
narrative_ontology:cs_axiom_status(covenant_extends_through_ishmael_line, holdable).
narrative_ontology:cs_axiom_grounding('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', covenant_extends_through_ishmael_line, theological).
narrative_ontology:cs_axiom('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', foundational, muhammad_completes_abrahamic_prophetic_succession).
narrative_ontology:cs_axiom_status(muhammad_completes_abrahamic_prophetic_succession, holdable).
narrative_ontology:cs_axiom_grounding('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', muhammad_completes_abrahamic_prophetic_succession, theological).
narrative_ontology:cs_axiom('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', secondary, genesis_ishmael_blessing_is_operative_not_incidental).
narrative_ontology:cs_axiom_status(genesis_ishmael_blessing_is_operative_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', genesis_ishmael_blessing_is_operative_not_incidental, theological).
narrative_ontology:cs_reference_frame('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', inclusive_abrahamic_transmission_order).
narrative_ontology:cs_drift_state('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', contemporary_pluralist_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0ce2f1a4-9db5-4f9d-a7dd-41c5de78ecb0', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, global_muslim_ummah).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_scholarly_establishment).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, arab_descendant_communities).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, converts_to_islam).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, rival_abrahamic_heirs).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, post_quranic_prophetic_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, global_muslim_ummah).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, abrahamic_lineage_continuity).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, finality_of_prophethood).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, quranic_confirmation_of_genesis_promises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nearly two billion people who understand themselves as heirs of Abraham through Ishmael's line. The covenant reading gives them a continuous sacred biography stretching from Abraham's prayers at the future site of Mecca to the present, a shared direction of prayer, festival calendar, and legal-ethical framework. Joining is open to anyone who affirms the confession; leaving carries severe social and, in some jurisdictions, legal consequences. Members both receive the identity goods and carry the compliance burdens the boundary maintenance demands.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, global_muslim_ummah, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__ishmael_covenant_reading, global_muslim_ummah, payer).

% The ulama, jurists, and transmitters who interpret Genesis through the Qur'an, adjudicate who stands inside the covenant, and maintain the doctrine that prophethood closed with Muhammad. Their social authority rests on custodianship of this reading: seminaries, courts, and endowments across fourteen centuries have been staffed and funded around its administration. Individual scholars can relocate into state bureaucracies or secular professions, but doing so forfeits the standing the custodianship confers.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_scholarly_establishment, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__ishmael_covenant_reading, islamic_scholarly_establishment, beneficiary).

% Arab peoples who claim descent from Ishmael and with it a primordial place in the sacred narrative: their language carries the final revelation, their geography hosts its central sanctuary. The reading confers prestige and centrality; it binds them to nothing they would not otherwise practice, and they move freely among national, sectarian, and secular identities without losing the descent claim.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, arab_descendant_communities, beneficiary,
    moderate, generational, mobile, regional).

% People who enter the community by affirming the confession, instantly acquiring full covenant membership that no birthline grants elsewhere in the Abrahamic field. Entry is deliberately frictionless; afterward, departure is socially catastrophic in much of the world and legally dangerous in some states, so the door opens wide inward and closes heavily behind.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, converts_to_islam, beneficiary,
    moderate, biographical, identity_locked, global).

% Jewish and Christian communities whose own covenant narrations precede and compete with this one. Where the Ishmael reading governs, their accounts are reframed as partial, corrupted, or superseded-in-error; historically they lived under contractual subordination (protected but second-class) legitimated partly by this covenantal hierarchy. They cannot stop being heirs of Abraham — the dispute follows them — but they retain their own texts, institutions, and in many states full civic equality.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, rival_abrahamic_heirs, payer,
    organized, generational, constrained, global).

% Movements arising from within the Islamic milieu that claim revelation or prophethood after Muhammad — the Ahmadiyya most prominently, with the Baha'i faith emerging adjacent. The finality clause built into this reading renders their central claim heresy by definition; in Pakistan they are constitutionally barred from calling themselves Muslims, and members face social ostracism, legal disability, and periodic violence. Recanting resolves the persecution at the price of the belief that constitutes them; persisting means carrying it.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, post_quranic_prophetic_claimants, payer,
    powerless, biographical, trapped, regional).

% Academic students of scripture, genealogy, and institutional authority who study the covenant readings side by side. They hold no stake in which transmission channel is true, publish on the rivalry's structure, and supply the outside corroboration the founding-problem record draws on.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__ishmael_covenant_reading, islamic_scholarly_establishment).
narrative_ontology:fixing_cost_class(abrahamic_covenant__ishmael_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains unity, continuity, and identity across a planetary community spanning every ethnicity and continent: one direction of prayer, one calendar, one prophetic biography connecting every member to Abraham — solved without a centralized church by embedding the covenant claim in scripture recited daily and lineage narrated in liturgy.
% TRANSFER_FUNCTION: Moves recognition and standing: confers Abrahamic heir-status on any who affirm the confession (inward, from outsiders), moves deference and institutional support to the scholarly custodians who administer the boundary (upward), and imposes recognition-costs outward — rival heirs' claims are demoted to error, post-Quranic prophetic claims are demoted to heresy.
% ABSENT_VOICES: Jewish and Christian self-narration enters the classical conversation only as the object of polemic, never as standing testimony with authority to describe its own covenant; post-Quranic prophetic movements are excluded from legitimacy-talk entirely rather than argued with; pre-Islamic Arabian religious memory survives only as filtered through Islamic sources. All three would describe the arrangement differently if seated as principals.
% DISAPPEARANCE_RATIONALE: Roughly a quarter of humanity organizes prayer, law, calendar, and self-narration around this transmission claim; its overnight removal would dissolve the ummah's connective identity, strand the scholarly establishments whose authority rests on it, and force every rival heir community to renegotiate its standing — the Abrahamic field would reorganize around the remaining readings.
% FOUNDING_PROBLEM: Seventh-century Arabia stood between exhausted empires with fragmented religious authority: local cults, scattered monotheisms, and no indigenous scripture or succession. The arrangement was built to solve the problem of continuity — who carries Abraham's inheritance forward, and on what authority — by declaring the transmission open through Ishmael and sealed in a final prophet who restores the original religion of Abraham.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary non-Muslim sources — Byzantine and Syriac chronicles recording the movement's emergence amid imperial war and religious flux — corroborate the sociological problem (fragmentation, succession crisis, imperial exhaustion) from outside the benefiting parties; modern secular historiography of late antiquity confirms the setting. The theological framing of the problem (human forgetting of revealed guidance requiring a final restatement) is attested only within the tradition itself; no external source attests that part.
narrative_ontology:disappearance_verdict(abrahamic_covenant__ishmael_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__ishmael_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__ishmael_covenant_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__ishmael_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__ishmael_covenant_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the arrangement's transfer runs in both directions — open admission pulls standing in, boundary enforcement pushes costs onto dissenters and rivals — and because the reading's own lights concede the enforcement costs it imposes. Suppression (0.6) reflects fourteen centuries of orthodoxy machinery: apostasy and blasphemy norms, the constitutional exclusion of Ahmadi claims in Pakistan, classical dhimma — averaged against secular polities where enforcement has lapsed. Theater is low (0.25): the narration is load-bearing (daily liturgy, pilgrimage to the associated sanctuary), not performative maintenance of a dead function, though the enforcement layer grows more theatrical where its machinery has lapsed. Accessibility collapse is low (0.3): the rival readings persist and flourish outside the reading's jurisdiction; accepting this reading collapses no alternative. Resistance is substantial (0.55): rival heirs actively maintain counter-readings, reformers contest the finality clause, and interfaith scholarship keeps the exclusivity question live. The measurement series run on one shared grid (points every 200 units of the interval, roughly centuries from c. 610 CE) so every tracked metric is authored at every examined time point; the trajectories show enforcement building through the classical period, peaking late, and partially decaying in the modern segment.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the scholarly establishment's position the arrangement is the continuity solution it administers — custodianship that confers standing and whose boundary work looks like fidelity. From the ummah's mass position it is identity infrastructure received at birth and rarely examined. From the rival heirs' position it is a competing deed to an inheritance they already hold, enforced where it governs. From the post-Quranic claimants' position it is a sealed door: the same finality clause that anchors the majority's continuity defines them as heresy. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the ummah, the scholarly establishment, Arab descendant communities, and converts all sit toward the subsidized end, though the ummah's dual position (receives identity goods, bears compliance burdens) and the establishment's fusion with the arrangement pull them off the pure-beneficiary pole. Victim declarations drive high directionality: rival heirs (constrained exit — heir-hood cannot be resigned) and especially post-Quranic claimants (trapped — recantation is the only exit and it costs the self) sit toward the full-target end, the claimants nearest it. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the arrangement's global scope, which amplifies the targets' effective burden because verification of fair dealing at planetary scale is weak.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — continuity of revelation and succession to Abraham's inheritance — remains live within the reading's own lights, so the arrangement is not mandate-dead, and the coordination function demonstrably still organizes a quarter of humanity. The mandatrophy risk is localized in the enforcement subsystem: where secular states have absorbed jurisdiction, the classical enforcement apparatus persists as ceremony and rhetoric while its machinery lapses — a piton-shaped residue inside a live tangled rope. Classifying the whole as a snare would erase the genuine, still-functioning coordination (and misread open admission as cover); classifying it as a rope would erase the documented costs to rival heirs and post-Quranic claimants. The tangled-rope claim keeps both halves visible, and the enforcement-layer-atrophy omega tracks whether the residue spreads.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the ishmael_covenant_reading of the abrahamic_covenant kernel; how would the classification shift under the sibling readings?',
    'Author the sibling files (isaac_covenant_reading, christian_supersessionist_reading, land_promise_constraint) and compare computed classifications; the disagreement is located in the transmission-channel premise — exclusive-Isaac, inclusive-Ishmael, superseded-by-Church, or territorial-grant — each of which redistributes the beneficiary and victim sets.',
    'Under the Isaac-exclusive reading the victim set expands (Ishmael''s line excluded outright); under supersessionism the beneficiary set contracts to the Church; under territorial readings a land-axis victim set appears. Epsilon and per-seat classifications move accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a multi-way contested kernel.').

omega_variable(
    inclusivity_boundary_depth,
    'Is the reading''s celebrated inclusivity structurally open (frictionless entry, porous interior) or bounded (entry gated by assent to finality-of-prophecy and lifelong legal obligation)?',
    'Compare conversion practice and exit data across jurisdictions; test whether assent to the finality clause (khatm al-nubuwwah) functions as an entry gate in practice rather than only in creed.',
    'If openness dominates, effective extraction falls toward coordination-only territory; if the boundary dominates, extraction rises toward the coercive end for dissenters and leavers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inclusivity_boundary_depth, empirical, 'Depth of the inclusion/exclusion trade-off inside the reading.').

omega_variable(
    suppression_regime_variance,
    'The scalar suppression value averages across wildly different enforcement regimes — classical dhimma, modern secular states, contemporary blasphemy-law states; is the operative suppression structural (statutes, courts), internalized (creedal certainty that finality is non-negotiable), or both?',
    'Stratify enforcement outcomes by regime type and track post-exit belief trajectories of leavers: persistence of boundary-enforcing attitudes after leaving jurisdictions with structural enforcement indicates the internalized share.',
    'If largely internalized, suppression travels with believers into secular states and the effective figure exceeds the structural measure; if largely structural, liberalizing polities collapse it quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_regime_variance, empirical, 'Structural vs internalized composition of boundary enforcement.').

omega_variable(
    genealogy_literal_vs_functional,
    'Does the coordination the reading provides depend on the literal genealogical claim (contemporary Arabs descend from Ishmael) or on its narrative function (a usable sacred biography linking a global community to Abraham)?',
    'Test whether devotional attachment tracks genealogical belief: survey believers who accept the narrative while doubting literal descent, and compare communities that retain the practice while dropping the genealogy.',
    'If functional, the reading''s coordination survives falsification of the descent claim and its classification is stable; if literal, the claim''s epistemic exposure becomes the arrangement''s exposure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genealogy_literal_vs_functional, conceptual, 'Whether the covenant reading''s load-bearing element is fact or narrative.').

omega_variable(
    enforcement_layer_atrophy,
    'In secular and diaspora settings the classical enforcement apparatus (juridical dhimma, apostasy jurisdiction) has lost its machinery while the covenant narration thrives — is the enforcement layer decaying into vestige, and is the arrangement drifting toward coordination-only operation?',
    'Track enforcement-capacity indicators (apostasy prosecutions, blasphemy statutes, dhimma analogues) against community growth across polities over coming decades.',
    'If enforcement atrophies globally, the arrangement migrates toward pure coordination and the extracted seats shrink toward the post-Quranic claimants alone; if enforcement re-hardens in majoritarian states, extraction resumes its historical accumulation trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_layer_atrophy, empirical, 'Whether the enforcement subsystem is vestigial or resurgent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ishmael_covenant_reading_tr_t0, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t200, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t400, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 400, 0.12).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t600, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 600, 0.15).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t800, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 800, 0.15).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t1000, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1000, 0.18).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t1200, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t1400, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1400, 0.25).

% Extraction over time
narrative_ontology:measurement(ishmael_covenant_reading_be_t0, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ishmael_covenant_reading_be_t200, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement(ishmael_covenant_reading_be_t400, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 400, 0.5).
narrative_ontology:measurement(ishmael_covenant_reading_be_t600, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(ishmael_covenant_reading_be_t800, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 800, 0.58).
narrative_ontology:measurement(ishmael_covenant_reading_be_t1000, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(ishmael_covenant_reading_be_t1200, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement(ishmael_covenant_reading_be_t1400, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1400, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ishmael_covenant_reading_su_t0, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(ishmael_covenant_reading_su_t200, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 200, 0.3).
narrative_ontology:measurement(ishmael_covenant_reading_su_t400, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 400, 0.42).
narrative_ontology:measurement(ishmael_covenant_reading_su_t600, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 600, 0.5).
narrative_ontology:measurement(ishmael_covenant_reading_su_t800, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 800, 0.55).
narrative_ontology:measurement(ishmael_covenant_reading_su_t1000, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement(ishmael_covenant_reading_su_t1200, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1200, 0.65).
narrative_ontology:measurement(ishmael_covenant_reading_su_t1400, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1400, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__ishmael_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Abrahamic covenant' decomposes, per the epsilon-invariance principle, into structurally distinct constraints: transmission-channel readings (this file; isaac_covenant_reading; christian_supersessionist_reading) and a distinct territorial-grant constraint (land_promise_constraint). Each carries its own epsilon because each redistributes the beneficiary and victim sets; forcing one story to span them would make epsilon observer-relative. Edges here link the family for contamination propagation: challenges to this reading's transmission claim propagate to the exclusivity and supersessionist readings, and to how the land promise may be inherited.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
