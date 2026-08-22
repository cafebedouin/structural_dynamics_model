% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__theistic_evolutionary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Theistic-Evolutionary Reading of Genesis 1-2 (Days as Epochs or Literary Framework)
 *   domain: religious/hermeneutical/science-religion
 *
 * SUMMARY:
 *   Within denominations, seminaries, and science-faith organizations that
 *   hold the theistic-evolutionary reading, Genesis 1-2 is taught as a
 *   theological framework: the text authoritatively teaches that God created,
 *   that creation is ordered and good, that humans bear the imago Dei, and
 *   that the dominion mandate is a stewardship ethic — while the days are
 *   read as epochs or a literary device, so the text is held to make no claim
 *   that conflicts with scientific cosmology or evolutionary biology. The
 *   arrangement is maintained by an interpretive apparatus: seminary
 *   curricula, denominational teaching and ordination standards,
 *   commentaries, and science-faith dialogue organizations. The apparatus
 *   solves a real forced-choice problem — members hold scientific credentials
 *   and congregational membership simultaneously — while concentrating
 *   interpretive authority in credentialed readers and ruling the plain-sense
 *   literal reading out of bounds within adopting communities. This story
 *   instantiates ONE reading of the genesis_creation_narrative kernel; the
 *   sibling readings are separate constraints with their own epsilon and
 *   stakeholder surfaces and are not adjudicated here (see kernel_context and
 *   the kernel_reading_contestation omega). KEY AGENTS (by structural
 *   relationship): - denominational_teaching_offices: agenda setter
 *   (institutional/arbitrage) — sets and enforces the reading, collects
 *   retention and credibility - theological_seminaries: primary beneficiary
 *   and receipt seat (institutional/constrained) — collects tuition,
 *   endowments, and the authority to certify interpreters; trains the
 *   enforcement layer - science_faith_dialogue_organizations: beneficiary
 *   (organized/identity_locked) — existence constituted by the harmony
 *   project - parish_clergy: beneficiary with payer costs
 *   (moderate/constrained) — pastoral tool; bears delivery labor and
 *   credibility risk - scientists_of_faith: beneficiary (moderate/mobile) —
 *   dual participation without partition - untrained_congregants: payer with
 *   beneficiary position (powerless/constrained) — receives reconciliation,
 *   pays deference - literalist_inclined_congregants: primary payer
 *   (powerless/constrained) — plain-sense reading ruled out; retrain, stay
 *   quiet, or exit - young_earth_apologetics_ministries: excluded rival
 *   (organized/arbitrage) — kept off teaching platforms -
 *   historians_of_science_and_religion: analytical observer
 *   (analytical/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__theistic_evolutionary, 0.48).
domain_priors:suppression_score(genesis_creation_narrative__theistic_evolutionary, 0.44).
domain_priors:theater_ratio(genesis_creation_narrative__theistic_evolutionary, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, extractiveness, 0.48).
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__theistic_evolutionary, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__theistic_evolutionary, "Theistic-Evolutionary Reading of Genesis 1-2 (Days as Epochs or Literary Framework)").
narrative_ontology:topic_domain(genesis_creation_narrative__theistic_evolutionary, "religious/hermeneutical/science-religion").

domain_priors:requires_active_enforcement(genesis_creation_narrative__theistic_evolutionary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__theistic_evolutionary, 'a12a2907-8acd-442a-bda2-17ba61c54440').
narrative_ontology:cs_kernel_codification('a12a2907-8acd-442a-bda2-17ba61c54440', fixed_text).
narrative_ontology:cs_authority_grounding('a12a2907-8acd-442a-bda2-17ba61c54440', expertise).
narrative_ontology:cs_interpretation_layer_present('a12a2907-8acd-442a-bda2-17ba61c54440').
narrative_ontology:cs_reading_relation('a12a2907-8acd-442a-bda2-17ba61c54440', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('a12a2907-8acd-442a-bda2-17ba61c54440', genesis_creation_narrative__allegorical_ancient_near_east, influences).
narrative_ontology:cs_axiom('a12a2907-8acd-442a-bda2-17ba61c54440', foundational, text_teaches_theology_not_science).
narrative_ontology:cs_axiom_status(text_teaches_theology_not_science, holdable).
narrative_ontology:cs_axiom_grounding('a12a2907-8acd-442a-bda2-17ba61c54440', text_teaches_theology_not_science, empirically_contingent).
narrative_ontology:cs_axiom('a12a2907-8acd-442a-bda2-17ba61c54440', foundational, divine_action_through_natural_process).
narrative_ontology:cs_axiom_status(divine_action_through_natural_process, holdable).
narrative_ontology:cs_axiom_grounding('a12a2907-8acd-442a-bda2-17ba61c54440', divine_action_through_natural_process, theological).
narrative_ontology:cs_axiom('a12a2907-8acd-442a-bda2-17ba61c54440', secondary, dominion_mandate_is_stewardship).
narrative_ontology:cs_axiom_status(dominion_mandate_is_stewardship, holdable).
narrative_ontology:cs_axiom_grounding('a12a2907-8acd-442a-bda2-17ba61c54440', dominion_mandate_is_stewardship, deontological).
narrative_ontology:cs_reference_frame('a12a2907-8acd-442a-bda2-17ba61c54440', theological_framework_genre_reading).
narrative_ontology:cs_drift_state('a12a2907-8acd-442a-bda2-17ba61c54440', contemporary_science_faith_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('a12a2907-8acd-442a-bda2-17ba61c54440', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__theistic_evolutionary, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, science_engaged_denominations).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, theological_seminaries).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, science_faith_dialogue_organizations).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, parish_clergy).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, scientists_of_faith).
narrative_ontology:constraint_victim(genesis_creation_narrative__theistic_evolutionary, literalist_inclined_congregants).
narrative_ontology:constraint_victim(genesis_creation_narrative__theistic_evolutionary, untrained_congregants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, untrained_congregants).
narrative_ontology:constraint_victim(genesis_creation_narrative__theistic_evolutionary, parish_clergy).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, compatibility_thesis).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, framework_hypothesis).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, day_age_harmonization).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, stewardship_dominion_ethic).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, imago_dei_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the curricula, ordination standards, and published teaching materials through which member congregations learn what Genesis 1-2 means. They adopted the framework reading as the authorized interpretation, require seminary training for teaching offices, and control both the reading's content and its enforcement. Their membership rolls and public standing depend on retaining scientifically educated members, which the reading delivers.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, denominational_teaching_offices, agenda_setter,
    institutional, generational, arbitrage, national).

% Train the clergy who deliver the reading and publish the commentaries that sustain it. Tuition, endowments, faculty positions, and the authority to certify interpreters all flow through them. A seminary that abandoned the framework reading would lose its denominational teaching contracts; one that taught only the plain sense would conflict with its accrediting denominations. Leaving the arrangement means reorienting the entire institution.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, theological_seminaries, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__theistic_evolutionary, theological_seminaries, agenda_setter).

% Exist to host and produce the harmony: conferences, books, grants, and public advocacy for the compatibility thesis. Their staff, donors, and public identity are constituted by the project; if the harmony were settled or dissolved, the organization would have no reason to exist. Leaving the project means dissolution rather than rebranding.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, science_faith_dialogue_organizations, beneficiary,
    organized, generational, identity_locked, continental).

% Deliver the reading weekly: answer the teenager who learned evolution at school, teach the framework from the pulpit, and absorb the credibility risk when members find the harmonization strained. The reading retains their scientifically minded members and gives them an answer to hard questions, but they carry the delivery labor and take the blame when it fails to persuade. Their career training binds them to the tradition's authorized reading.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, parish_clergy, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__theistic_evolutionary, parish_clergy, payer).

% Work in evolutionary biology, geology, or cosmology while holding congregational membership. The reading lets them hold both commitments without mental partition. Their professional standing never depended on the reading, so they can leave the faith or the reading at low professional cost.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, scientists_of_faith, beneficiary,
    moderate, biographical, mobile, global).

% Sit under the reading without the training to evaluate it. They are told the days were never meant as ordinary days — that the plain sense they bring to the text is not the meaning — and that the authorized reading requires expertise they do not have. They receive the reconciliation, keeping both their faith and their trust in science, and pay for it in deference to credentialed interpreters.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, untrained_congregants, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__theistic_evolutionary, untrained_congregants, beneficiary).

% Read the text the way it presents itself to an untrained reader — six days, recent creation — and are told that reading is out of bounds in their own community. Their options are retraining themselves into the authorized reading, staying quiet, or leaving for a tradition that teaches the plain sense, at the cost of community, family, and sometimes identity.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, literalist_inclined_congregants, payer,
    powerless, biographical, constrained, local).

% Run the rival reading at scale — museums, curricula, media organizations — and are excluded from adopting communities' teaching platforms. They contest the framework reading as eisegesis and compete for the same constituencies; their exclusion from the room is maintained by the same teaching standards that authorize the framework reading.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, young_earth_apologetics_ministries, excluded,
    organized, generational, arbitrage, national).

% Study the reading's history from outside any adopting community: they document when the harmonizations changed, what each adjustment conceded, and how the apparatus's institutional interests shaped what counted as faithful interpretation. They publish the record the apparatus presents as settled.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, historians_of_science_and_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__theistic_evolutionary, theological_seminaries).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__theistic_evolutionary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the faith-science forced-choice problem for communities holding both scriptural authority and scientific cosmology: one interpretive discipline under which members can hold scientific credentials and congregational membership simultaneously, and under which the text's theological teaching (creation by God, creation ordered and good, the imago Dei, the sabbath principle) is preserved while the days are read as epochs or literary device.
% TRANSFER_FUNCTION: Moves interpretive authority from untrained readers to credentialed interpreters — the plain-sense reading is ruled out of bounds, and the authorized reading requires seminary training to see. Moves deference and interpretive labor from congregants to the teaching apparatus. Moves member retention, public credibility, tuition, and funding to denominations, seminaries, and dialogue organizations.
% ABSENT_VOICES: Literalist-inclined members sit in the pews of adopting communities but are absent from the teaching conversation: their reading is classified as a problem to be corrected before any discussion starts. Young-earth apologetics ministries are excluded from adopting communities' educational platforms. Secular critics who read the harmonization as unmotivated accommodation are likewise outside the room. Each would contest the reading's central premise from a different direction.
% DISAPPEARANCE_RATIONALE: If the theistic-evolutionary reading and its apparatus vanished overnight, the faith-science forced choice would reopen: scientifically educated members would again face exit from faith communities or cognitive partition; mainline denominations would lose the retention and public credibility the reading provides; seminary curricula and dialogue organizations would lose their curricular center. The literal and allegorical sibling readings would compete to absorb the displaced population, and the science-religion interface would reorganize around whichever won.
% FOUNDING_PROBLEM: The Darwin-era crisis: after 1859, communities holding scriptural authority faced defection of their scientifically educated members and public ridicule, while the plain-sense reading of Genesis 1-2 (recent creation, sequential six-day manufacture) collided with geological deep time and evolutionary biology. The theistic-evolutionary reading was built to end the forced choice: preserve the text's doctrinal authority and the community's membership by reading the days as epochs or literary device.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: young-earth creationist ministries — organized opponents of this reading — attest the conflict is live, since their own institutions are built around the same collision (they resolve it by the opposite move); contemporary accounts of the fundamentalist controversy and of science-driven disaffiliation document the founding crisis; sociologists of religion studying retention and disaffiliation attest both the problem's persistence and this reading's retention effects. No party disputes that the conflict exists; the live dispute is over which resolution the text itself permits.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__theistic_evolutionary, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__theistic_evolutionary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__theistic_evolutionary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__theistic_evolutionary, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__theistic_evolutionary, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__theistic_evolutionary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_narrative__theistic_evolutionary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_narrative__theistic_evolutionary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. I claim tangled_rope from structure: the same apparatus that coordinates (resolves the faith-science forced choice, preserving dual membership for the science-engaged majority) also extracts (concentrates interpretive authority in credentialed readers, rules the plain-sense reading out of bounds, and is funded by the perceived necessity of the harmonization), and the arrangement requires active enforcement — without curricula, ordination standards, and teaching norms, untrained readers drift back to the plain sense. Metrics are authored descriptively. Extractiveness 0.48: the referent is the standing arrangement under contest (the interpretive apparatus as it operates), never the literal or allegorical alternatives; the value nets the genuine coordination delivered against the real costs — deference concentration, literalist marginalization, apparatus maintenance — that this reading's own honest practitioners acknowledge. Suppression 0.44, authored raw and unscaled by design (only extractiveness is scaled by directionality and scope): enforcement is real but bounded — curricular gating, ordination standards, teaching-platform exclusion — with no state coercion, open exit to rival traditions, and effectively zero suppression of scientific consensus (the reading is defined by deference to science; what is suppressed is rival hermeneutics inside adopting communities). The mechanism is mostly structural (institutional gating) with an internalized component (members trained to treat the plain sense as embarrassing self-censor; see the hermeneutic_boundary_suppression_scope omega). Theater 0.32: compatibility is partly ritually asserted ('no conflict' as a boundary marker) while the specific harmonization migrates (day-age to framework hypothesis to analogical days), but the exegetical labor is real. Accessibility_collapse 0.35: once trained into the reading, the plain-sense option collapses for that reader, but the allegorical sibling remains fully live and the literal reading persists outside adopting communities. Resistance 0.55: organized young-earth ministries contest the reading as eisegesis, secular critics attack it as unfalsifiable accommodation, and literalist-inclined members resist internally — their effective coalition channel is exit to rival traditions rather than internal organization, since internal coalition would require contesting credentialed authority without credentials. Measurement series run on one shared time grid (t in years since 1859: t0 = 1859, t160 = 2019), all three metrics authored at all nine points.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the teaching office the arrangement is the faithful resolution of a real crisis: without it, the denomination loses its scientifically educated members. From the seminary seat it is a discipline that requires training — the plain sense is simply mistaken, and deference to expertise is how truth works. From the untrained congregant's seat it is relief purchased with deference: no forced choice, but no authority to read for themselves either. From the literalist-inclined member's seat it is gatekeeping: their reading ruled out before the conversation starts, by people whose livelihood the ruling protects. The two powerless payer seats differ from each other despite the same power atom and exit class: untrained_congregants hold a genuine beneficiary position (they keep both commitments), while literalist_inclined_congregants hold almost none — the differentiator is what the reading does to each seat's prior interpretive commitment. The dialogue organizations add an identity-lock dynamic: their self-concept has fused with the harmony project, so they experience any threat to the reading's necessity as existential rather than positional; if that identity frame broke — if the harmony were settled enough to need no professional maintainers — their seat would vacate rather than migrate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats sit near the beneficiary end: denominational_teaching_offices (they set the reading and collect its retention and credibility dividends), theological_seminaries (tuition, endowments, and the certification pipeline flow to them), science_faith_dialogue_organizations (identity-fused collectors), parish_clergy and scientists_of_faith (net collectors with real secondary costs — clergy's delivery labor and credibility risk, captured in their secondary payer role). Payer seats sit near the target end: literalist_inclined_congregants are near-full targets — the arrangement's enforcement operates directly on their reading. Untrained_congregants are declared victims (they bear the deference cost through the same structure that coordinates everyone else) but carry a genuine secondary beneficiary position, so their effective extraction is damped relative to the pure payer seat; the derivation should read their victim declaration together with their secondary role. Suppression is authored raw and unscaled; extractiveness is what the engine scales by directionality and scope — most seats operate at local-to-national scope, with the scientists' seat global.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the Darwin-era forced choice between scriptural authority and scientific education — is still live: new scientific fields keep arriving and the conflict keeps regenerating, which is why the apparatus persists as function rather than inertia. No mandatrophy is declared. The tangled_rope classification prevents two mislabelings: calling this a pure rope would erase the extraction (the deference concentration and the ruled-out plain sense are real costs, not zero coordination overhead); calling it a snare would erase the coordination (the forced-choice problem is real, the arrangement genuinely solves it for most participants, and there is no identifiable class that only loses). Watch condition: if the founding problem ever died — if the science-faith conflict dissolved — the apparatus would persist on institutional inertia and its theater ratio would climb; the R5 mismatch consumer should treat founding_problem_status=dead combined with disappearance_verdict=world_rearranges as the piton-transition signal. The dialogue organizations are the identity-lock canary: their exit is dissolution, so they hold the strongest structural incentive to keep the problem 'live' regardless of whether it is — their attestation of the founding problem's liveness should always be weighted against that incentive, which is why the corroboration record leans on their opponents and on disaffiliation studies instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the theistic_evolutionary reading of the genesis_creation_narrative kernel; the sibling readings (literal_young_earth, allegorical_ancient_near_east) instantiate different constraints over the same text — what exactly do they change structurally, and where is the disagreement located?',
    'Author and compile the sibling stories and compare per-seat classifications across the family: literal_young_earth should author high suppression aimed at scientific consensus itself, with creation-science ministries as beneficiaries and science-educated members as payers; allegorical_ancient_near_east should dissolve the compatibility apparatus and shift authority to the literary-critical guild.',
    'The disagreement is located in the days'' referent and the text''s claim-status. If the sibling classifications diverge sharply from this one, the kernel''s readings are genuinely distinct constraints and epsilon-invariance holds across the family; if they converge, the kernel label is doing less structural work than the contest suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one reading of a three-reading kernel; siblings are separate constraints, not internal hedges.').

omega_variable(
    compatibility_discovered_or_maintained,
    'Is the compatibility of Genesis 1-2 with scientific cosmology a stable discovered property of the text, or an achievement continuously maintained by interpretive labor that must be renewed as science moves?',
    'Track the harmonization apparatus''s adjustment rate against scientific change across the interval: if each major scientific advance (deep time, natural selection, genetics, neuroscience) required renewed interpretive adjustment, compatibility is maintenance-dependent.',
    'If maintenance-dependent, the arrangement drifts toward transitional status and rising theater as adjustments accumulate; if stable, the coordination function dominates and the extraction estimates should fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compatibility_discovered_or_maintained, empirical, 'Whether the harmony is a found fact of the text or a continuously maintained achievement of the apparatus.').

omega_variable(
    interpretive_complexity_necessity,
    'Is the concentration of interpretive authority in credentialed readers a necessary feature of the text''s actual genre complexity (Hebrew philology, ANE literary convention), or an apparatus effect — complexity sustained because it sustains the authority?',
    'Examine whether faithful simplified readings persist among untrained readers when the apparatus withholds enforcement: congregational reading practices without curricular reinforcement, and the historical record of pre-apparatus lay reading.',
    'If complexity is apparatus-sustained, the deference cost measures extraction above what the text requires and the payer seats'' burden is inflated; if genuinely necessary, the deference is coordination cost and effective extraction falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_complexity_necessity, empirical, 'Whether the expertise requirement is textually necessary or institutionally convenient.').

omega_variable(
    hermeneutic_boundary_suppression_scope,
    'The reading''s enforcement suppresses rival hermeneutics (the plain-sense reading) within adopting communities while deferring to scientific consensus — how much of the measured suppression is structural (curricular gating, ordination standards, platform exclusion) and how much internalized (members trained to treat their own reading as embarrassing)?',
    'Compare self-censorship and exit behavior among literalist-inclined members before and after adopting communities tighten teaching standards; post-exit suppression trajectory — if the self-censorship persists after leaving for a rival tradition, the internalized component is substantial.',
    'If internalized, effective suppression is higher than the structural measure suggests and travels with members who exit; if purely structural, the authored scalar is accurate as it stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_boundary_suppression_scope, empirical, 'Structural vs internalized split of the enforcement burden on plain-sense readers.').

omega_variable(
    day_referent_textual_underdetermination,
    'Can the internal evidence of the text settle whether the days are epochs, a literary framework, or ordinary days, or does the decision require extra-textual commitment (to scientific consensus, to an inerrancy doctrine) — and does this reading''s stability therefore depend on its host communities'' scientific deference rather than on the text?',
    'Philological and literary analysis conducted independently of doctrinal commitment: if the text underdetermines the referent, the choice among readings is driven by external commitments and the reading family''s structure follows the distribution of those commitments.',
    'If underdetermined, this reading''s persistence is a function of its host communities'' deference to science, and the sibling readings are differently committed rather than simply mistaken — shifting the family''s classification logic from error-correction to commitment-mapping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(day_referent_textual_underdetermination, conceptual, 'Whether the days'' referent is decidable from the text alone or requires extra-textual commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__theistic_evolutionary, 0, 160).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t20, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 20, 0.13).
narrative_ontology:measurement_basis(gene_tr_t20, observed).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(gene_tr_t40, observed).
narrative_ontology:measurement(gene_tr_t60, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(gene_tr_t60, observed).
narrative_ontology:measurement(gene_tr_t80, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 80, 0.26).
narrative_ontology:measurement_basis(gene_tr_t80, observed).
narrative_ontology:measurement(gene_tr_t100, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 100, 0.29).
narrative_ontology:measurement_basis(gene_tr_t100, observed).
narrative_ontology:measurement(gene_tr_t120, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 120, 0.31).
narrative_ontology:measurement_basis(gene_tr_t120, observed).
narrative_ontology:measurement(gene_tr_t140, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 140, 0.32).
narrative_ontology:measurement_basis(gene_tr_t140, observed).
narrative_ontology:measurement(gene_tr_t160, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 160, 0.32).
narrative_ontology:measurement_basis(gene_tr_t160, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t20, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(gene_be_t20, observed).
narrative_ontology:measurement(gene_be_t40, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 40, 0.33).
narrative_ontology:measurement_basis(gene_be_t40, observed).
narrative_ontology:measurement(gene_be_t60, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(gene_be_t60, observed).
narrative_ontology:measurement(gene_be_t80, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 80, 0.48).
narrative_ontology:measurement_basis(gene_be_t80, observed).
narrative_ontology:measurement(gene_be_t100, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 100, 0.5).
narrative_ontology:measurement_basis(gene_be_t100, observed).
narrative_ontology:measurement(gene_be_t120, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 120, 0.51).
narrative_ontology:measurement_basis(gene_be_t120, observed).
narrative_ontology:measurement(gene_be_t140, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 140, 0.5).
narrative_ontology:measurement_basis(gene_be_t140, observed).
narrative_ontology:measurement(gene_be_t160, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 160, 0.48).
narrative_ontology:measurement_basis(gene_be_t160, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t20, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 20, 0.16).
narrative_ontology:measurement_basis(gene_su_t20, observed).
narrative_ontology:measurement(gene_su_t40, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 40, 0.24).
narrative_ontology:measurement_basis(gene_su_t40, observed).
narrative_ontology:measurement(gene_su_t60, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 60, 0.36).
narrative_ontology:measurement_basis(gene_su_t60, observed).
narrative_ontology:measurement(gene_su_t80, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 80, 0.42).
narrative_ontology:measurement_basis(gene_su_t80, observed).
narrative_ontology:measurement(gene_su_t100, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 100, 0.44).
narrative_ontology:measurement_basis(gene_su_t100, observed).
narrative_ontology:measurement(gene_su_t120, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 120, 0.45).
narrative_ontology:measurement_basis(gene_su_t120, observed).
narrative_ontology:measurement(gene_su_t140, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 140, 0.44).
narrative_ontology:measurement_basis(gene_su_t140, observed).
narrative_ontology:measurement(gene_su_t160, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 160, 0.44).
narrative_ontology:measurement_basis(gene_su_t160, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__theistic_evolutionary, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_narrative__theistic_evolutionary, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__theistic_evolutionary, genesis_creation_narrative__allegorical_ancient_near_east).

% DUAL FORMULATION NOTE:
% This story is one member of a three-reading family over the genesis_creation_narrative kernel (a fixed text). The epsilon-invariance principle requires separate stories because the readings instantiate structurally different constraints over the same text: literal_young_earth authors high suppression aimed at scientific consensus itself, with creation-science ministries as beneficiaries and science-educated members as victims; allegorical_ancient_near_east dissolves the compatibility apparatus entirely and shifts the authority structure to the literary-critical guild; this reading authors moderate extraction over the harmonization arrangement itself. Sibling constraint_ids are presumed per the kernel's naming convention (genesis_creation_narrative__literal_young_earth, genesis_creation_narrative__allegorical_ancient_near_east). The upstream/downstream structure runs both ways: this reading's harmonization concessions legitimize the genre-critical methods the allegorical reading builds on (influences edge), while the literal reading is logically excluded by this reading's core premise (forecloses edge).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
