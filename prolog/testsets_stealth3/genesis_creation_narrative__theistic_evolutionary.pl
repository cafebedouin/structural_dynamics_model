% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__theistic_evolutionary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__theistic_evolutionary, []).

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
 *   constraint_id: genesis_creation_narrative__theistic_evolutionary
 *   human_readable: Theistic-Evolutionary Reading of Genesis 1-2: Days as Epochs or Literary Framework
 *   domain: religious/hermeneutical/science-religion-interface
 *
 * SUMMARY:
 *   Within communities that hold Genesis 1-2 authoritative, the
 *   theistic-evolutionary reading installs a hermeneutic discipline: the
 *   opening chapters teach theology (ordered creation, imago Dei, sabbath,
 *   stewardship) through ancient idiom, the days are epochs or a literary
 *   framework rather than a chronometer, and evolutionary cosmology is
 *   theologically permissible — indeed welcome. The reading is administered,
 *   not merely held: seminaries train clergy in genre discernment, ordination
 *   processes expect it, denominational statements and curricula circulate
 *   it, and dedicated harmonization organizations maintain its public face.
 *   EPSILON REFERENT: the standing arrangement under contest is the
 *   interpretive regime governing Genesis 1-2 in adopting communities — the
 *   text exercising normative authority through authorized interpretation —
 *   assessed by this reading's own lights. Because this reading endorses the
 *   text's authority, epsilon is moderate-low: it registers the discipline
 *   the framework imposes (epistemic deference from laity, continuous
 *   harmonization labor from clergy, ruling-out of literalist readings)
 *   netted against the reconciliation the framework delivers. FAMILY NOTE:
 *   this is one of three linked decompositions of the kernel
 *   genesis_creation_narrative. The literal_young_earth sibling carries high
 *   epsilon (suppression of scientific consensus; believers bear
 *   pseudoscience-maintenance costs); the allegorical_ancient_near_east
 *   sibling carries low epsilon but drops revelatory authority, shifting
 *   beneficiaries toward academic scholarship. This reading sits between:
 *   genuine coordination with concentrated deference extraction. All three
 *   files link via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - mainline_theological_establishment: Primary administrator (institutional/identity_locked) — trains clergy, sets ordination standards, captures the interpretive-authority rents
 *   - reconciled_lay_believers: Primary beneficiary (moderate/mobile) — receives faith-science reconciliation and continued belonging
 *   - scientific_community: Secondary beneficiary (institutional/arbitrage) — gains cultural legitimacy in adopting populations, bears no reciprocal cost
 *   - literalist_minorities: Primary target (organized/mobile, override d=0.80) — their reading is what the framework rules out; their exit was adversarial secession, not arbitrage
 *   - plain_sense_lay_readers: Secondary target (powerless/constrained) — bear the cost of expert-mediated meaning against surface sense
 *   - clergy_harmonizers: Dual-positioned laborer (moderate/identity_locked) — bears the harmonization labor, collects vocational coherence
 *   - doubting_youth_in_adopting_churches: Excluded voice (powerless/mobile) — object silently, by leaving
 *   - academic_biblical_scholars: Analytical observer (institutional/analytical) — supplies the genre and reception evidence all parties use
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__theistic_evolutionary, 0.36).
domain_priors:suppression_score(genesis_creation_narrative__theistic_evolutionary, 0.28).
domain_priors:theater_ratio(genesis_creation_narrative__theistic_evolutionary, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, extractiveness, 0.36).
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__theistic_evolutionary, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__theistic_evolutionary, "Theistic-Evolutionary Reading of Genesis 1-2: Days as Epochs or Literary Framework").
narrative_ontology:topic_domain(genesis_creation_narrative__theistic_evolutionary, "religious/hermeneutical/science-religion-interface").

domain_priors:requires_active_enforcement(genesis_creation_narrative__theistic_evolutionary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__theistic_evolutionary, '738b870b-1def-47fc-aac8-2a4d591b4c0b').
narrative_ontology:cs_kernel_codification('738b870b-1def-47fc-aac8-2a4d591b4c0b', fixed_text).
narrative_ontology:cs_authority_grounding('738b870b-1def-47fc-aac8-2a4d591b4c0b', lineage).
narrative_ontology:cs_interpretation_layer_present('738b870b-1def-47fc-aac8-2a4d591b4c0b').
narrative_ontology:cs_reading_relation('738b870b-1def-47fc-aac8-2a4d591b4c0b', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('738b870b-1def-47fc-aac8-2a4d591b4c0b', genesis_creation_narrative__allegorical_ancient_near_east, coexists_with).
narrative_ontology:cs_axiom('738b870b-1def-47fc-aac8-2a4d591b4c0b', foundational, divine_accommodation_hermeneutic).
narrative_ontology:cs_axiom_status(divine_accommodation_hermeneutic, holdable).
narrative_ontology:cs_axiom_grounding('738b870b-1def-47fc-aac8-2a4d591b4c0b', divine_accommodation_hermeneutic, theological).
narrative_ontology:cs_axiom('738b870b-1def-47fc-aac8-2a4d591b4c0b', foundational, two_books_harmony_doctrine).
narrative_ontology:cs_axiom_status(two_books_harmony_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('738b870b-1def-47fc-aac8-2a4d591b4c0b', two_books_harmony_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('738b870b-1def-47fc-aac8-2a4d591b4c0b', accommodationist_two_books_framework).
narrative_ontology:cs_drift_state('738b870b-1def-47fc-aac8-2a4d591b4c0b', contemporary_secularizing_pressure, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('738b870b-1def-47fc-aac8-2a4d591b4c0b', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__theistic_evolutionary, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, mainline_theological_establishment).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, reconciled_lay_believers).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, scientific_community).
narrative_ontology:constraint_victim(genesis_creation_narrative__theistic_evolutionary, literalist_minorities).
narrative_ontology:constraint_victim(genesis_creation_narrative__theistic_evolutionary, plain_sense_lay_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, clergy_harmonizers).
narrative_ontology:constraint_victim(genesis_creation_narrative__theistic_evolutionary, clergy_harmonizers).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, two_books_harmony_doctrine).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, divine_accommodation_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seminaries, denominational bodies, and senior theologians who administer the reading: they train clergy in genre discernment, set ordination expectations, publish the commentaries and curricula through which the framework circulates, and issue public statements affirming compatibility with evolutionary science. Their institutional continuity and public credibility depend on the framework's continued operation; leaving it would mean surrendering the mediating office that defines them.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, mainline_theological_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Members of adopting communities who experience the framework as resolving a personal conflict: they keep their faith commitments while accepting evolutionary biology and cosmology without felt contradiction. What flows to them is cognitive peace and continued communal belonging; what they give up is unmediated access to the text's surface meaning. Leaving would mean choosing between the community and the intellectual life they currently experience as compatible.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, reconciled_lay_believers, beneficiary,
    moderate, biographical, mobile, global).

% Biologists, geologists, and cosmologists whose work the framework accommodates rather than contests. In adopting populations they gain cultural legitimacy for evolution education and relief from religious obstruction; they bear no reciprocal obligation and proceed regardless of the framework's fortunes.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, scientific_community, beneficiary,
    institutional, civilizational, arbitrage, global).

% Young-earth and creationist believers inside or adjacent to adopting denominations. The framework rules their reading out of bounds: seminaries will not teach it, ordination boards screen for it, and denominational publications treat it as an embarrassment. Many have already exited to found parallel institutions such as creationist ministries and separate schools; those who remain inside adopt a quietist stance. Their reading survives only outside the framework's jurisdiction.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, literalist_minorities, payer,
    organized, generational, mobile, global).

% Ordinary members who read the opening chapters the way they appear to read — six days, ordered sequence, recent humanity — and are told by trained interpreters that the surface sense is not the teaching. They bear the cost of accepting an expert-mediated meaning they did not choose and cannot independently verify, on pain of seeming unsophisticated in their own community.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, plain_sense_lay_readers, payer,
    powerless, biographical, constrained, national).

% Pastors, catechists, and religious educators who must master the framework well enough to answer congregants' questions, teach it to youth, and absorb the friction when the harmonization feels strained. The interpretive labor is theirs; the vocational benefit of having coherent answers is also theirs. Exit would mean abandoning a profession whose current form presupposes the framework.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, clergy_harmonizers, payer,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__theistic_evolutionary, clergy_harmonizers, beneficiary).

% Adolescents and young adults raised inside adopting communities who find the harmonization strained — neither the literal story they half-remember nor the science they learn feels like the whole truth — and who rarely voice the difficulty before drifting away. Their objections surface statistically, in retention data, rather than in denominational assemblies.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, doubting_youth_in_adopting_churches, excluded,
    powerless, biographical, mobile, national).

% University-based scholars of the Hebrew Bible and of religion who study the interpretive landscape comparatively — their genre analyses, reception histories, and sociologies supply the evidentiary background every party draws on, while they themselves collect nothing from the framework's operation.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, academic_biblical_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__theistic_evolutionary, mainline_theological_establishment).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__theistic_evolutionary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides adopting communities a shared hermeneutic that resolves the collision between scriptural authority and scientific cosmology: genre discernment (days as epochs or a literary framework) lets members affirm deep time, common descent, and cosmic evolution while retaining Genesis 1-2 as revelatory; it coordinates clergy formation, curricula, and public testimony so the community engages science coherently instead of fragmenting into schism or silent defection.
% TRANSFER_FUNCTION: Moves interpretive authority and epistemic deference upward from lay readers to credentialed interpreters (seminaries, denominational bodies, theologians); moves cultural legitimacy outward to scientific institutions within believing populations; concentrates marginalization costs on literalist minorities whose reading the framework rules out.
% ABSENT_VOICES: Rank-and-file members are governed by hermeneutic policy they never vote on — seminaries and denominational bodies set the reading, and pew-level dissent surfaces mainly as silent departure (see doubting_youth_in_adopting_churches). Literalist minorities speak loudly but are received as outvoted outsiders within adopting institutions. Secular critics who regard the entire harmonization project as unfalsifiable accommodation are heard but granted no standing inside the framework's own adjudication.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, adopting communities would split along the fault line it covers: a fundamentalist exit wing reclaiming literal chronology and a secular-drift wing abandoning the text's authority altogether; clergy would lose the answer-set their vocation currently presupposes; science-faith conflict would reignite in school boards, bioethics, and public rhetoric; and the millions currently holding both commitments would be forced to choose. The rearrangement would be large, fast, and observable — the signature of arrangements that depend on the constraint.
% FOUNDING_PROBLEM: Nineteenth-century geology's deep time and Darwin's common descent collided with the traditional literal chronology of Genesis 1-2, threatening communities determined to keep both scriptural authority and intellectual respectability; the framework was built to let believers retain the text as authoritative revelation without rejecting the science.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of science document the Victorian crisis of faith and the late-nineteenth-century denominational splits over Darwinism; creationist adversaries attest the collision is real — they resolve it oppositely, discarding evolution rather than literalism; and secular sociology of religion documents ongoing science-faith tension as a defection driver. The problem predates the framework and is attested loudest by its opponents, which is the strongest available provenance.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__theistic_evolutionary, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__theistic_evolutionary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__theistic_evolutionary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__theistic_evolutionary, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__theistic_evolutionary, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__theistic_evolutionary_tests).
:- end_tests(genesis_creation_narrative__theistic_evolutionary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.36: the framework's transfers are real but bounded — deference concentrates interpretive authority in credentialed hands, harmonization labor falls on clergy, and literalist minorities bear marginalization — yet the delivered reconciliation is substantive, so participants are broadly net beneficiaries. Suppression 0.28 (raw and unscaled — only extractiveness is scaled by directionality and scope): the framework does not suppress scientific consensus (that is the reading's defining delta); its suppressive force is intra-communal — ordination screening, curriculum control, social cost for plain-sense insistence — and it is partly internalized (a trained reflex that surface reading is naive), an ambiguity carried by omega suppression_mechanism_split. Theater 0.24: concordist boilerplate and no-conflict badges exist, but the underlying genre work is real. Accessibility_collapse 0.45: alternatives persist — literalism thrives in adjacent jurisdictions, and exit to secular life or other traditions remains open — so the framework does not close the option space. Resistance 0.40: creationist counter-movements, lay grumbling at elite hermeneutics, and silent youth defection. CLAIMED TYPE tangled_rope, independent of the metrics: the same structure that coordinates (a shared hermeneutic letting communities hold scripture and science together) also extracts asymmetrically (deference flows up, marginalization costs fall on literalists and plain-sense readers), and it requires active enforcement — soft enforcement through formation and gatekeeping rather than coercion, visible in the suppression_requirement series, which rises from 0.15 (1859, voluntary accommodation) to 0.40 (1950, magisterial caution and seminary gatekeeping after the modernist crises) and decays to 0.28 (2025, normalization and cultural transmission replacing enforcement). Identity-coordination gaming check: the identity_coordination typing could excuse coupling that concentrates extraction on powerless agents at large scope; here the concentration is real (plain_sense_lay_readers, powerless, national scope) and is priced into the tangled_rope claim and the 0.36 epsilon rather than excused by the complexity offset. All series share one time grid (1859, 1925, 1950, 1980, 2000, 2025) so no metric row borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the establishment seat the framework is faithful mediation it built and staffed — a near-rope experience with low personal burden. From the literalist seat the same structure is enforced exclusion — their reading is the enforcement object, and that seat computes snare-flavored despite the framework's soft methods. From the plain-sense seat it is epistemic dispossession: told the text they read does not mean what it says. From the scientific seat it is costless legitimation. An outside skeptical seat sees unfalsifiable retreat. The engine derives these divergences from power, exit, and directional position; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. mainline_theological_establishment (beneficiary, identity_locked) derives near the beneficiary end — the framework subsidizes its office. reconciled_lay_believers (beneficiary, mobile) derive low d. scientific_community (beneficiary, arbitrage) derives nearest the beneficiary end — subsidy without exposure. plain_sense_lay_readers (victim, powerless, constrained) derive high d — trapped deference amplifies their effective extraction. doubting_youth sit near-symmetric: they receive the framework's teaching yet bear its strain, and their exit is silent departure. OVERRIDE: literalist_minorities carry a directionality override (organized, d=0.80). The structural derivation would damp their d toward the middle because they possess demonstrated exit capacity — but their exit was adversarial secession (parallel institution-building outside the framework's jurisdiction), not arbitrage through it; within any contact with adopting institutions they sit at full target, since the framework's operation consists precisely in ruling their reading out. The override restores the target-side position the exit-dampening would erase. clergy_harmonizers are left to derivation: their dual position (payer labor, beneficiary vocation) nets mildly target-side, and the engine's dual-role handling prices the ambivalence better than a hard override would.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy declaration: the founding problem — the collision between scriptural authority and scientific cosmology — is live, corroborated by adversaries as well as historians. The framework has not outlived its function. Two decay paths are watched rather than asserted: first, if literalism exits the population and science-faith peace becomes unremarkable, the mediating apparatus could persist as ritual affirmation with theater_ratio climbing toward piton territory; second, if omega accommodation_vs_discovery resolves as pure retreat-tracking, the framework's authority claim decays toward the allegorical sibling rather than toward inertia. The measurement series (theater flat-to-declining since 1980, suppression decaying) currently shows neither path dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel genesis_creation_narrative (reading: theistic_evolutionary). What would each sibling reading change structurally if instantiated instead?',
    'Comparative classification across the three sibling stories using identical structural probes — the same stakeholder seats re-rolled under each reading''s beneficiary and victim declarations.',
    'Under literal_young_earth, suppression rises sharply (scientific consensus becomes the suppressed alternative), scientists and educators join the victim set, and epsilon increases substantially. Under allegorical_ancient_near_east, revelatory authority drops, the establishment loses its mediating rent, beneficiaries migrate toward academic scholarship, and the arrangement approaches a low-extraction scholarly convention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one of three readings of the Genesis 1-2 kernel; sibling readings instantiate different constraints with different victim sets and epsilon.').

omega_variable(
    suppression_mechanism_split,
    'Is the framework''s residual suppression structural (ordination gates, curriculum control, denominational discipline) or internalized (the trained reflex that surface reading is naive, persisting after barriers are removed)?',
    'Post-exit suppression trajectory: track members who leave adopting traditions for secular or literalist communities; if the deference reflex and genre-suspicion persist after the enforcing institutions are gone, reclassify as partially internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure and the post-1980 enforcement decay overstates liberalization; the constraint travels with members after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural vs. internalized suppression mechanism in a hermeneutical constraint.').

omega_variable(
    accommodation_vs_discovery,
    'Is the non-literal reading a discovery about the text''s genre and intent, or a post-hoc retreat that redraws the text''s claims wherever science advances?',
    'Weight pre-Darwin non-literal exegesis (Philo, Augustine''s non-literal days) for internal-textual motivation against the post-1859 redrawing record; then test responsiveness to future scientific surprise — does the framework ever constrain science, or only accommodate it?',
    'If pure accommodation-chasing, the framework''s authority claim erodes toward the allegorical sibling, deference extraction loses its discovery warrant, and epsilon rises; if genuinely discoverable in the text, the coordination function stands on firmer ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_vs_discovery, conceptual, 'Whether the reading tracks the text or chases science — the classic accommodation objection.').

omega_variable(
    retention_efficacy,
    'Does the framework actually retain the doubters it is built to hold, or does it merely delay defection while transferring the crisis to adolescence?',
    'Longitudinal cohort religiosity data comparing adopting and non-adopting communities, controlling for general secularization; examine whether doubt surfaces earlier or later and whether total retention differs.',
    'If retention is ineffective, the coordination function is weaker than claimed, theater_ratio is understated, and the arrangement drifts toward theatrical maintenance of a peace it no longer delivers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retention_efficacy, empirical, 'Whether the reconciliation product is real or deferred defection.').

omega_variable(
    deference_benefit_balance,
    'Do reconciled lay believers experience net benefit, or is the peace purchased with a deference burden they would not accept if the tradeoff were explicit — and who is entitled to decide what counts as benefit here?',
    'Participatory valuation: elicit believers'' own tradeoff assessments (belonging versus unmediated textual access) rather than expert welfare judgments; compare across education strata.',
    'If lay members themselves price the deference as a loss, reconciled_lay_believers migrate toward the payer set, the beneficiary structure thins, and the classification moves from tangled_rope toward snare at the lay seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deference_benefit_balance, preference, 'Welfare-framing dependence of the beneficiary declaration for lay adherents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__theistic_evolutionary, 1859, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genesis_te_tr_t1859, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 1859, 0.1).
narrative_ontology:measurement_basis(genesis_te_tr_t1859, observed).
narrative_ontology:measurement(genesis_te_tr_t1925, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 1925, 0.16).
narrative_ontology:measurement_basis(genesis_te_tr_t1925, observed).
narrative_ontology:measurement(genesis_te_tr_t1950, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 1950, 0.21).
narrative_ontology:measurement_basis(genesis_te_tr_t1950, observed).
narrative_ontology:measurement(genesis_te_tr_t1980, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 1980, 0.27).
narrative_ontology:measurement_basis(genesis_te_tr_t1980, observed).
narrative_ontology:measurement(genesis_te_tr_t2000, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 2000, 0.26).
narrative_ontology:measurement_basis(genesis_te_tr_t2000, observed).
narrative_ontology:measurement(genesis_te_tr_t2025, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 2025, 0.24).
narrative_ontology:measurement_basis(genesis_te_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(genesis_te_be_t1859, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 1859, 0.2).
narrative_ontology:measurement_basis(genesis_te_be_t1859, observed).
narrative_ontology:measurement(genesis_te_be_t1925, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 1925, 0.34).
narrative_ontology:measurement_basis(genesis_te_be_t1925, observed).
narrative_ontology:measurement(genesis_te_be_t1950, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement_basis(genesis_te_be_t1950, observed).
narrative_ontology:measurement(genesis_te_be_t1980, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement_basis(genesis_te_be_t1980, observed).
narrative_ontology:measurement(genesis_te_be_t2000, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement_basis(genesis_te_be_t2000, observed).
narrative_ontology:measurement(genesis_te_be_t2025, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 2025, 0.36).
narrative_ontology:measurement_basis(genesis_te_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(genesis_te_su_t1859, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 1859, 0.15).
narrative_ontology:measurement_basis(genesis_te_su_t1859, observed).
narrative_ontology:measurement(genesis_te_su_t1925, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 1925, 0.35).
narrative_ontology:measurement_basis(genesis_te_su_t1925, observed).
narrative_ontology:measurement(genesis_te_su_t1950, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement_basis(genesis_te_su_t1950, observed).
narrative_ontology:measurement(genesis_te_su_t1980, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 1980, 0.37).
narrative_ontology:measurement_basis(genesis_te_su_t1980, observed).
narrative_ontology:measurement(genesis_te_su_t2000, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 2000, 0.31).
narrative_ontology:measurement_basis(genesis_te_su_t2000, observed).
narrative_ontology:measurement(genesis_te_su_t2025, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 2025, 0.28).
narrative_ontology:measurement_basis(genesis_te_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__theistic_evolutionary, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_narrative__theistic_evolutionary, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__theistic_evolutionary, genesis_creation_narrative__allegorical_ancient_near_east).

% DUAL FORMULATION NOTE:
% One kernel (the interpretive status of Genesis 1-2), three readings, three constraints. The literal_young_earth sibling carries high epsilon: it suppresses scientific consensus and believers bear the cost of maintaining pseudoscience. The allegorical_ancient_near_east sibling carries low epsilon but drops revelatory authority, shifting beneficiaries toward academic scholarship and stripping the framework of its consolations. This reading sits between: genuine coordination with concentrated deference extraction. Upstream/downstream: literal_young_earth is the historical default from which this reading diverged under geological and evolutionary pressure; allegorical_ancient_near_east is the academic downstream that radicalizes the genre move by dropping authority. Each family file links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genesis_creation_narrative__theistic_evolutionary, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
