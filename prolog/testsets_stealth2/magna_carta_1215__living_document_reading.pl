% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta Living-Document Reading: Interpretive Tradition as Constitutional Authority
 *   domain: constitutional law/legal history/political theory
 *
 * SUMMARY:
 *   The living-document reading construes Magna Carta not as a fixed set of
 *   1215 bargains but as an adaptive constitutional substrate: the charter's
 *   authority persists because each generation's interpreters — Coke's
 *   Institutes, the Petition of Right, the Habeas Corpus Act, the settlement
 *   of 1689, and their transatlantic descendants — legitimately supersede
 *   original meaning with accumulated precedent. As a constraint, it governs
 *   interpretive authority itself: it binds all governed subjects to meanings
 *   no original party consented to, while licensing the interpreter class to
 *   revise those meanings continuously. Per the epsilon-invariance principle,
 *   the colloquial label 'Magna Carta' decomposes into three structurally
 *   distinct constraints; this file instantiates only the
 *   living_document_reading. The baronial_privilege_reading (feudal contract
 *   limited to contracting landowning parties) and the
 *   universal_rights_reading (Clause 39 emitting transhistorical universal
 *   due process) are separate stories linked through
 *   network.affects_constraints, with epsilon values that differ from this
 *   one by wide margins. KEY AGENTS (by structural relationship): -
 *   common_law_judiciary: Primary agenda-setter and principal beneficiary
 *   (institutional/identity_locked) — administers the tradition and collects
 *   interpretive authority - professional_legal_academy: Secondary
 *   beneficiary (organized/identity_locked) — transmits and certifies the
 *   doctrine - constitutional_reform_movements: Beneficiary
 *   (organized/mobile) — harvests the tradition's legitimating vocabulary -
 *   executive_government: Dual-positioned payer/beneficiary
 *   (institutional/constrained) — bound by precedents it did not choose,
 *   exploiting flexibility where doctrine is unsettled -
 *   originalist_interpretive_dissenters: Primary target
 *   (moderate/identity_locked) — methodologically disenfranchised within the
 *   frame - subjects_bound_by_unconsented_precedent: Target
 *   (powerless/trapped) — bound by evolved meanings they cannot ratify or
 *   reject - direct_democracy_advocates: Excluded voice (organized/trapped) —
 *   would demand ratification channels that do not exist -
 *   constitutional_historians: Analytical observer (analytical/analytical) —
 *   documents departure from the historical record without adjudicative force
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.47).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.44).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta Living-Document Reading: Interpretive Tradition as Constitutional Authority").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional law/legal history/political theory").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '45c93754-610b-4f90-8951-d3b426edc218').
narrative_ontology:cs_kernel_codification('45c93754-610b-4f90-8951-d3b426edc218', fixed_text).
narrative_ontology:cs_authority_grounding('45c93754-610b-4f90-8951-d3b426edc218', lineage).
narrative_ontology:cs_interpretation_layer_present('45c93754-610b-4f90-8951-d3b426edc218').
narrative_ontology:cs_reading_relation('45c93754-610b-4f90-8951-d3b426edc218', magna_carta_1215__baronial_privilege_reading, influences).
narrative_ontology:cs_reading_relation('45c93754-610b-4f90-8951-d3b426edc218', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('45c93754-610b-4f90-8951-d3b426edc218', foundational, interpretive_supersession_is_legitimate).
narrative_ontology:cs_axiom_status(interpretive_supersession_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('45c93754-610b-4f90-8951-d3b426edc218', interpretive_supersession_is_legitimate, conventional).
narrative_ontology:cs_axiom('45c93754-610b-4f90-8951-d3b426edc218', foundational, precedential_accumulation_constitutes_development).
narrative_ontology:cs_axiom_status(precedential_accumulation_constitutes_development, holdable).
narrative_ontology:cs_axiom_grounding('45c93754-610b-4f90-8951-d3b426edc218', precedential_accumulation_constitutes_development, empirically_contingent).
narrative_ontology:cs_reference_frame('45c93754-610b-4f90-8951-d3b426edc218', living_tradition_adaptive_substrate).
narrative_ontology:cs_drift_state('45c93754-610b-4f90-8951-d3b426edc218', contemporary_originalist_challenge, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('45c93754-610b-4f90-8951-d3b426edc218', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, professional_legal_academy).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_reform_movements).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, originalist_interpretive_dissenters).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, subjects_bound_by_unconsented_precedent).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, executive_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, executive_government).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, common_law_doctrinal_continuity).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, whig_constitutional_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides what the charter means in each case, layering ruling upon ruling; each judgment becomes part of the body of precedent that future courts treat as authoritative. Judges are bound by the very tradition they extend — stare decisis constrains them as much as it empowers them — and their professional standing rests on faithful stewardship of accumulated doctrine. Exit would mean repudiating the craft they embody.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, common_law_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, common_law_judiciary, beneficiary).

% Trains each cohort of lawyers into the doctrinal tradition, writes the casebooks and commentaries through which precedent is transmitted, and staffs the commissions and inquiries that certify constitutional change. Careers and intellectual identities are built inside the tradition; leaving it means leaving the discipline.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, professional_legal_academy, beneficiary,
    organized, generational, identity_locked, continental).

% Invoke the charter's accumulated meanings to legitimize new claims — extending old guarantees to new subjects and circumstances. The tradition's openness is their resource: they can appeal to precedent without amending anything. When the tradition closes against a claim they redirect to statutes or rights instruments.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_reform_movements, beneficiary,
    organized, generational, mobile, national).

% Is bound by judicial constructions of ancient liberties — prerogative narrowed by precedents it did not choose — while also exploiting interpretive flexibility where precedent is unsettled, expanding practice first and leaving courts to ratify or restrain it afterward. Its position alternates between paying and profiting depending on which branch of doctrine is in play.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, executive_government, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, executive_government, beneficiary).

% Scholars and jurists committed to recovering the 1215 text's original meaning and measuring later doctrine against it. Their method is professionally legible but structurally disadvantaged: within the tradition's own terms, their findings arrive as critique, not authority. Adopting the tradition's method would dissolve the position that defines them.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_interpretive_dissenters, payer,
    moderate, biographical, identity_locked, continental).

% Live under constitutional meanings evolved across eight centuries by institutions they cannot vote out of interpretive office and cannot formally ratify or reject. Their obligations track the current state of doctrine, whatever it is; opting out is not available short of emigration or revolution.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, subjects_bound_by_unconsented_precedent, payer,
    powerless, biographical, trapped, national).

% Would require popular ratification for constitutional change rather than elite interpretive evolution. They hold no seat in the interpretive process — no referendum channel, no formal veto — and their proposals surface only as occasional political pressure that the tradition absorbs without structural concession.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, direct_democracy_advocates, excluded,
    organized, generational, trapped, national).

% Study the charter's transmission and transformation from outside the interpretive professions, documenting where doctrine departs from the historical record. Their findings inform debate but carry no adjudicative force.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_historians, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__living_document_reading, common_law_judiciary).
narrative_ontology:fixing_cost_class(magna_carta_1215__living_document_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains constitutional continuity across radically changed circumstances: a fixed medieval text remains authoritative because each generation's interpreters adapt its application, so the polity avoids choosing between rigid literalism and wholesale abandonment of its founding commitments.
% TRANSFER_FUNCTION: Moves interpretive authority from the charter's original parties (long dead) to successive interpreter classes — courts, jurists, parliaments — and moves compliance from all governed subjects to whatever meanings those interpreters currently endorse.
% ABSENT_VOICES: The original contracting parties are eight centuries dead and cannot contest what their bargain has become; contemporary non-professional publics have no seat in doctrinal formation; direct-democracy advocates would demand ratification channels that do not exist. Unanimity about the tradition's legitimacy arises within a room built by and for the tradition's practitioners.
% DISAPPEARANCE_RATIONALE: If the living-tradition constraint vanished overnight, eight centuries of doctrine — the due-process lineage, habeas corpus development, the whole edifice of evolved meaning — would lose its warrant simultaneously. Constitutional orders would reorganize around either textual fundamentalism (binding only what 1215 said) or explicit amendment politics (nothing binds unless ratified); the common-law world's entire mode of constitutional development would have to be rebuilt.
% FOUNDING_PROBLEM: How can a short medieval treaty-charter continue to bind and guide a modern polity whose circumstances its drafters could not imagine — without either discarding it as obsolete or freezing it into irrelevance?
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the benefiting professions (the Holt/Maitland line) attest both the founding problem and the reality of the adaptive solution; the repeated royal reissues of 1216–1225 and the 1297 confirmation — parliamentary and chancery records, not interpreter self-report — corroborate that the charter repeatedly failed to govern without renewal. Originalist scholars corroborate that the problem is live while disputing that interpretive supersession is the right answer.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.47: the arrangement genuinely coordinates (constitutional continuity without amendment machinery) while concentrating interpretive authority in a self-perpetuating class and binding subjects to meanings no party consented to — substantial but not predatory, and moderated in this reading's own assessment by the legitimacy the tradition confers. Suppression 0.44 reflects enforcement that is professional and epistemic rather than physical: courts apply precedent regardless of interpretive dissent and career incentives punish methodological defection, but rival methods remain publishable and politically arguable. Theater_ratio 0.20: most activity is functional adjudication; the ceremonial share (jubilees, anniversary invocations, symbolic citation) is real but secondary. Accessibility_collapse 0.35: accepting the living-document frame does not collapse alternatives — originalism persists as a live minority method — but it demotes those alternatives to critique rather than authority. Resistance 0.50: sustained scholarly and political resistance, insufficient to displace the frame. The temporal series shares one ten-point grid (alignment rule): extractiveness climbs with doctrinal concentration through the Cokean revival and nineteenth-century consolidation, then plateaus and eases slightly as democratic legitimation and academic pluralism spread; theater humps at 1770 with Whig mythologizing and recedes as historiography corrects the record; suppression_requirement traces enforcement-capacity build-up (weak early enforcement, judicial enforcement maturing after 1600, post-1689 entrenchment) then partial relaxation as statutory absorption reduced the need for active defense. Claim and metrics are authored independently: the claim is tangled_rope because both a genuine coordination function and asymmetric extraction are structurally present and actively enforced; the metrics describe operation without reference to the claim.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat the constraint is its own craft: discretionary, self-renewing, experienced as freedom-under-tradition rather than extraction. From the originalist dissenters' seat the same structure operates as methodological disenfranchisement — their findings can never constitute authority within the frame, only commentary upon it. From the subjects' seat it is binding without consent: obligations track doctrine they had no part in making. Same structure, three different computed types; the engine derives this divergence from power, exit options, and declared position, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidized end: the judiciary collects interpretive authority (partially offset by its own subjection to the precedent it extends), the academy collects careers and certifying power, reform movements collect a legitimating vocabulary usable without amendment. Declared targets sit near the full-target end: originalist dissenters are identity-locked into a method the constraint defines as critique rather than authority, and subjects are trapped — obligations follow doctrine they cannot ratify or reject. The executive is genuinely dual-positioned (pays when precedent narrows prerogative, profits when flexibility permits expansion); its derived directionality sits nearer the target end on the strength of its victim declaration, which is the honest center of gravity of its position. No directionality_overrides are authored: the override mechanism keys on power atoms, and the judiciary and executive share the institutional atom, so any correction calibrated for one would contaminate the other; the residual ambiguity is carried instead in the executive's dual-role declaration and in this commentary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live — inherited constitutional texts still require adaptation to circumstances their drafters could not imagine — so there is no decayed mandate to resolve and mandatrophy is not declared. The tangled_rope classification does protective work in both directions: it prevents a pure-extraction reading that would erase the genuine coordination function (continuity across eight centuries without amendment machinery), and it prevents a pure-coordination reading that would conceal the interpreter class's concentrated authority and the unconsented binding of subjects. The theater hump around 1770 (Whig mythologizing of the charter) is documented in the temporal series but recedes afterward; the constraint shows no drift toward inertial performance, and the accountability and supersession-limit omegas flag the paths along which degradation would show up if it began.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the living_document_reading of kernel magna_carta_1215. What structural deltas would the sibling readings (baronial_privilege_reading, universal_rights_reading) produce if instantiated?',
    'Compile and classify all three reading files side by side; compare beneficiary/victim sets, directionality distributions, and computed types across the kernel.',
    'The baronial reading narrows beneficiaries to landed elites and makes all non-parties targets, raising measured extraction sharply; the universal reading relocates authority from accumulated tradition to a transhistorical principle, shrinking the interpreter class''s rents and changing the victim set to everyone outside due-process protection. Cross-reading comparison is the intended output of the kernel decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame omega: which reading of the Magna Carta kernel this story instantiates and what siblings would change.').

omega_variable(
    accumulation_tracks_development,
    'Does precedential accumulation actually constitute development — does the tradition track normative improvement — or does it accumulate error and retrogression at comparable rates?',
    'Longitudinal doctrinal audit: sample settled precedents across the interval, score each against contemporaneous and retrospective normative assessment, and compute the improvement rate of the accumulated stock.',
    'If accumulation is progress-neutral or negative, the reading''s core justification (adaptation as development) fails and the arrangement''s extractive share rises — the coordination story thins toward cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accumulation_tracks_development, empirical, 'Whether the tradition''s growth is genuinely developmental, the empirical premise underwriting this reading.').

omega_variable(
    interpreter_accountability_depth,
    'Is the interpreter class effectively accountable to the governed for the meanings it produces, or does interpretive authority escape democratic control behind professional gatekeeping?',
    'Trace the accountability channels (appointment, parliamentary override, statutory correction, professional discipline) and measure how often and how effectively they have corrected doctrine the governed opposed.',
    'If accountability is nominal, extraction concentrates further in the judiciary seat and the computed type shifts toward the snare end of the tangled_rope range; robust accountability supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpreter_accountability_depth, empirical, 'Depth of democratic control over evolved constitutional meaning.').

omega_variable(
    supersession_limit_location,
    'Does this reading admit any limit on what interpretive tradition may supersede — can accumulated precedent legitimately rewrite any original provision, or only some class of provisions?',
    'Conceptual analysis of the reading''s own practice: identify whether courts and jurists treat any clauses as unsupersedable anchors (e.g., the 1225 reissue''s retained core) and articulate the principled basis, or conclude the limit is ad hoc.',
    'A principled limit would strengthen the coordination function (tradition as disciplined development); an ad hoc or absent limit would expose the reading to the charge that development is whatever the interpreter class last did, raising effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_limit_location, conceptual, 'Location of the boundary of legitimate supersession within the living-document frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 1215, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__living_document_reading, theater_ratio, 1215, 0.08).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_1215__living_document_reading, theater_ratio, 1300, 0.12).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_1215__living_document_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement(magn_tr_t1600, magna_carta_1215__living_document_reading, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_1215__living_document_reading, theater_ratio, 1689, 0.22).
narrative_ontology:measurement(magn_tr_t1770, magna_carta_1215__living_document_reading, theater_ratio, 1770, 0.3).
narrative_ontology:measurement(magn_tr_t1860, magna_carta_1215__living_document_reading, theater_ratio, 1860, 0.28).
narrative_ontology:measurement(magn_tr_t1950, magna_carta_1215__living_document_reading, theater_ratio, 1950, 0.24).
narrative_ontology:measurement(magn_tr_t2010, magna_carta_1215__living_document_reading, theater_ratio, 2010, 0.21).
narrative_ontology:measurement(magn_tr_t2026, magna_carta_1215__living_document_reading, theater_ratio, 2026, 0.2).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__living_document_reading, base_extractiveness, 1215, 0.12).
narrative_ontology:measurement(magn_be_t1300, magna_carta_1215__living_document_reading, base_extractiveness, 1300, 0.18).
narrative_ontology:measurement(magn_be_t1400, magna_carta_1215__living_document_reading, base_extractiveness, 1400, 0.22).
narrative_ontology:measurement(magn_be_t1600, magna_carta_1215__living_document_reading, base_extractiveness, 1600, 0.38).
narrative_ontology:measurement(magn_be_t1689, magna_carta_1215__living_document_reading, base_extractiveness, 1689, 0.44).
narrative_ontology:measurement(magn_be_t1770, magna_carta_1215__living_document_reading, base_extractiveness, 1770, 0.49).
narrative_ontology:measurement(magn_be_t1860, magna_carta_1215__living_document_reading, base_extractiveness, 1860, 0.53).
narrative_ontology:measurement(magn_be_t1950, magna_carta_1215__living_document_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(magn_be_t2010, magna_carta_1215__living_document_reading, base_extractiveness, 2010, 0.47).
narrative_ontology:measurement(magn_be_t2026, magna_carta_1215__living_document_reading, base_extractiveness, 2026, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__living_document_reading, suppression_requirement, 1215, 0.25).
narrative_ontology:measurement(magn_su_t1300, magna_carta_1215__living_document_reading, suppression_requirement, 1300, 0.2).
narrative_ontology:measurement(magn_su_t1400, magna_carta_1215__living_document_reading, suppression_requirement, 1400, 0.22).
narrative_ontology:measurement(magn_su_t1600, magna_carta_1215__living_document_reading, suppression_requirement, 1600, 0.4).
narrative_ontology:measurement(magn_su_t1689, magna_carta_1215__living_document_reading, suppression_requirement, 1689, 0.48).
narrative_ontology:measurement(magn_su_t1770, magna_carta_1215__living_document_reading, suppression_requirement, 1770, 0.5).
narrative_ontology:measurement(magn_su_t1860, magna_carta_1215__living_document_reading, suppression_requirement, 1860, 0.52).
narrative_ontology:measurement(magn_su_t1950, magna_carta_1215__living_document_reading, suppression_requirement, 1950, 0.46).
narrative_ontology:measurement(magn_su_t2010, magna_carta_1215__living_document_reading, suppression_requirement, 2010, 0.43).
narrative_ontology:measurement(magn_su_t2026, magna_carta_1215__living_document_reading, suppression_requirement, 2026, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Magna Carta' per the epsilon-invariance principle. The single natural-language concept covers three structurally distinct claims with materially different epsilon values: (1) baronial_privilege_reading — a feudal contract whose protection set is limited to contracting landowning parties (historically anchored, narrow beneficiary set, high exclusion extraction); (2) living_document_reading (this file) — a meta-constraint on interpretive authority in which accumulated precedent legitimately supersedes original meaning (genuine coordination function plus interpreter-class rents); (3) universal_rights_reading — a transhistorical rights precedent in which Clause 39 emits universal due process independent of interpretive evolution (authority relocated from tradition to principle, shrinking interpreter rents). The upstream historical scholarship feeding readings 1 and 3 is cited as evidence within reading 2's tradition; each story links the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
