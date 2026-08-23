% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command (Deut 7) — Contextual Supersession Reading
 *   domain: religious/hermeneutical
 *
 * SUMMARY:
 *   Deuteronomy 7 commands the destruction of the Canaanite nations and
 *   forbids intermarriage with them. This story instantiates one reading of
 *   that kernel — the contextual-supersession reading — under which the
 *   directive was bound to ancient Israel's settlement moment and has been
 *   morally retired by the later universalist strain of the prophets and, in
 *   Christian hands, by the new covenant. The standing arrangement this story
 *   measures is the text's operative force under that reading: retained in a
 *   closed canon, taught as belonging to another age, governing present
 *   conduct only where the teaching's reach fails. Claim and metrics are
 *   authored independently: the reading is claimed as tangled_rope — a
 *   genuine coordination achievement (keeping scripture whole while retiring
 *   a command no one may follow) that nonetheless asymmetrically burdens
 *   identifiable seats — while the metrics describe a hybrid whose
 *   enforcement has decayed for two millennia without dying, whose
 *   performative share now exceeds its functional share, and whose victim set
 *   has narrowed to enclave-held minorities and, on one strand, to the Jewish
 *   communities the covenant-transfer version of supersession historically
 *   billed. Assumptions: the interval maps the interpretive arc from the
 *   settlement-era enforcement regime (T=0) to the contemporary post-Shoah
 *   settlement (T=30); metric values are the author's structural judgments,
 *   not instrument readings.
 *
 * KEY AGENTS:
 *   - mainline_denominational_authorities: agenda-setter and collector of standing (institutional/arbitrage) — certifies the retirement, controls ordination and curricula, gains continuity and moral cover
 *   - mainstream_congregants: beneficiary (moderate/mobile) — receive a usable ethic and a whole canon, bear almost nothing
 *   - intermarried_and_converted_members: beneficiary (moderate/constrained) — admitted on belief and consent where the reading governs
 *   - enclave_intermarried_and_dissenters: primary residual target (powerless/trapped) — face shunning and expulsion where the reading's reach fails
 *   - jewish_communities_under_covenant_transfer: strand-dependent target (organized/constrained) — bear election-transfer costs under the Christian-covenant strand
 *   - literalist_clergy_in_mainline_bodies: identity-locked target (moderate/identity_locked) — vocation-priced dissent inside the certifying bodies
 *   - secular_canonical_critics: excluded (organized/mobile) — object to retention itself, hold no seat in confessional adjudication
 *   - academic_hermeneutics_scholars: analytical observer (institutional/analytical) — document the reading's history and costs
 *   - targeted_canaanite_populations: historical bearer, memorial non-agent seat — bore the arrangement entire and cannot check any reading of it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.32).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.48).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command (Deut 7) — Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious/hermeneutical").

domain_priors:requires_active_enforcement(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, 'aaff7a35-f457-466f-a6a9-22c454480c4b').
narrative_ontology:cs_kernel_codification('aaff7a35-f457-466f-a6a9-22c454480c4b', fixed_text).
narrative_ontology:cs_authority_grounding('aaff7a35-f457-466f-a6a9-22c454480c4b', lineage).
narrative_ontology:cs_interpretation_layer_present('aaff7a35-f457-466f-a6a9-22c454480c4b').
narrative_ontology:cs_reading_relation('aaff7a35-f457-466f-a6a9-22c454480c4b', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('aaff7a35-f457-466f-a6a9-22c454480c4b', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('aaff7a35-f457-466f-a6a9-22c454480c4b', foundational, command_binding_indexed_to_historical_office).
narrative_ontology:cs_axiom_status(command_binding_indexed_to_historical_office, holdable).
narrative_ontology:cs_axiom_grounding('aaff7a35-f457-466f-a6a9-22c454480c4b', command_binding_indexed_to_historical_office, theological).
narrative_ontology:cs_axiom('aaff7a35-f457-466f-a6a9-22c454480c4b', foundational, prophetic_universalist_ethic_governs_present_conduct).
narrative_ontology:cs_axiom_status(prophetic_universalist_ethic_governs_present_conduct, holdable).
narrative_ontology:cs_axiom_grounding('aaff7a35-f457-466f-a6a9-22c454480c4b', prophetic_universalist_ethic_governs_present_conduct, deontological).
narrative_ontology:cs_reference_frame('aaff7a35-f457-466f-a6a9-22c454480c4b', historically_bounded_settlement_directive).
narrative_ontology:cs_drift_state('aaff7a35-f457-466f-a6a9-22c454480c4b', contemporary_post_shoah_dialogue, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('aaff7a35-f457-466f-a6a9-22c454480c4b', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, mainstream_congregants).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, intermarried_and_converted_members).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, mainline_denominational_authorities).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, enclave_intermarried_and_dissenters).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, jewish_communities_under_covenant_transfer).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, literalist_clergy_in_mainline_bodies).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, progressive_revelation_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, moral_supersession_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and certify how Deuteronomy 7 is taught across their seminaries, pulpits, and curricula: ordination candidates must demonstrate the historically-bounded reading, curriculum boards retire lesson plans that apply the ban to present-day neighbors, and interfaith statements reaffirm the universalist ethic. They keep the text in the canon and the canon in continuous use, gaining institutional continuity and moral standing from holding both at once. Exit is easy in form — they could rejoin a stricter or looser reading coalition — but costly in standing.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mainline_denominational_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, mainline_denominational_authorities, beneficiary).

% Sit in communities where the ban on the nations is taught as belonging to another age: they inherit a full canon and a clear conscience together, may marry across ethnic lines without ceremony or penalty, and encounter the hard text mainly as an annual reading with an attached explanation. Nothing is asked of them by the old directive, and little is asked of them by the new settlement beyond accepting the explanation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mainstream_congregants, beneficiary,
    moderate, biographical, mobile, global).

% Enter or remain in the community across the ethnic line the old directive drew: spouses of differing background, converts of any origin, children of mixed marriages. Where the supersessionist teaching governs, their membership runs on shared belief and consent rather than descent. Their exposure is local — in communities that reject the teaching, the same marriage can cost them standing or kinship.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, intermarried_and_converted_members, beneficiary,
    moderate, biographical, constrained, global).

% Live inside communities that never accepted the retirement of the old directive: members who marry out face shunning or expulsion, doubters who voice it lose family and livelihood together, and the door back is guarded by the same kinship that makes leaving ruinous. They bear costs the wider tradition declares obsolete, in places the wider tradition's teaching offices do not reach.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, enclave_intermarried_and_dissenters, payer,
    powerless, biographical, trapped, regional).

% Carry the cost that appears when the superseding agent is the Christian covenant rather than the prophets: if Israel's commission has passed to the Church, continuing Judaism reads as a relic, and historic consequences — dispossession, forced sermons, conversionary pressure — followed that teaching. Organized and articulate, with a diaspora's depth, but unable to exit the argument, since the claim concerns their covenant itself. Postwar dialogues have won formal retractions from several churches, unevenly kept.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, jewish_communities_under_covenant_transfer, payer,
    organized, generational, constrained, global).

% Ordained into bodies whose teaching offices they now doubt: convinced the old directive states a standing standard, they cannot preach it without losing pulpit, pension, and colleagues, and cannot unbelieve it without losing the calling that organizes their life. Exit exists — stricter denominations would receive them — but it is priced as the loss of identity, not just employment.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, literalist_clergy_in_mainline_bodies, payer,
    moderate, biographical, identity_locked, continental).

% Stand outside the confessional conversation and object to its terms: from their seat the problem is not how to read the ban but why a text commanding it remains in any community's canon at all. They publish, testify, and debate, but the adjudication they would change happens in ordination committees and synods where they hold no seat.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, secular_canonical_critics, excluded,
    organized, biographical, mobile, global).

% Trace the reading's history and bill: dating the sources, documenting the rabbinic neutralizations, tallying the covenant-transfer consequences, comparing enclave discipline rates. They take no side in the communities' dispute and can be cited by every party, which is why each party cites them selectively.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, academic_hermeneutics_scholars, observer,
    institutional, generational, analytical, global).

% The peoples the directive named: in the historical arrangement they bore it entire — land, cult, and life — and left no descendants seated at any table where the text is now interpreted. Kept here as a memorial seat: every reading of the kernel is answerable to a party that cannot check the answer.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, targeted_canaanite_populations, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__contextual_supersession_reading, targeted_canaanite_populations).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__contextual_supersession_reading, mainline_denominational_authorities).
narrative_ontology:fixing_cost_class(herem_command_dt7__contextual_supersession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a closed canon coherent while relocating the directive's force outside present conduct: the community retains the whole text (continuity, shared reference, liturgical wholeness) and settles boundary questions — who may marry in, who counts as neighbor — on belief and consent rather than descent, using the later universalist writings as the operative ethic.
% TRANSFER_FUNCTION: Moves interpretive authority and moral standing to the teaching offices that certify the retirement; moves the retention's costs to literalist clergy (career pricing), to enclave members who face discipline the wider tradition calls obsolete, and — where the superseding agent is the Christian covenant — to Jewish communities billed with a superseded covenant; moves admission outward, granting intermarried and converted members standing the older settlement denied.
% ABSENT_VOICES: The peoples the directive named have no seat in any reading's conversation and cannot check what any reading says about them. Closer in: secular critics of canon-retention stand outside the ordination committees and synods where the arrangement is actually adjudicated; exiters from enclave enforcement usually leave before their testimony registers; and in Christian bodies deciding what Israel's covenant means, Jewish interlocutors are often consulted after the decision rather than seated within it.
% DISAPPEARANCE_RATIONALE: If the supersessionist settlement vanished overnight, the text's force would revert to the strongest remaining reading in each community: enclave enforcement would normalize as merely consistent, allegorizing would expand unmoored from any historical check, and excision pressure would return from the left; the mainline's working peace — whole canon, quiet conscience — collapses into open contest, and intermarriage norms in the affected communities tighten within a generation.
% FOUNDING_PROBLEM: How a community whose canon contains a command of total war against named peoples can keep treating that canon as authoritative scripture without practicing or endorsing the command — how to keep the text and lose the order.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: post-Shoah Jewish-Christian dialogue documents (Nostra Aetate and successor Protestant statements) attest from outside the Christian teaching offices that the covenant-transfer strand required formal retraction; academic herem scholarship, across confessional and secular seats, attests the interpretive problem remains live; published testimony of exiters from enforcing enclaves attests the residual coercion is real. No source outside the teaching offices attests that the problem is settled — that attestation exists only within the offices that certify the retirement.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.32: near-zero on intermarriage where the reading governs (membership runs on belief and consent), but materially above zero because the arrangement still bills identifiable seats — enclave members who face discipline the teaching offices declare obsolete, mainline literalist clergy whose careers price their conviction, and, on the covenant strand, Jewish communities whose continuing covenant the transfer-version delegitimates. Suppression is 0.48, authored as a raw structural property and never scaled: the arrangement holds its middle by active means — ordination gates, curriculum review, denominational discipline against both flank positions — but the machinery is lighter than its historical predecessors. Theater_ratio is 0.55: the largest single activity the arrangement now performs around this text is explaining why it does not bind, a performance that grows as the function shrinks; the series shows theater crossing 0.5 in the final third of the interval, the Goodhart signature of proxy maintenance replacing application. Accessibility_collapse is 0.5: alternatives are partly available — the allegorical approach flourishes beside this one, enclaves keep the literal practice, secular exit exists — but canon excision has been effectively closed since the second century. Resistance is 0.55: post-Shoah interfaith corrections, scholarly herem criticism, and enclave defiance are real and have won formal concessions. The identity_coordination declaration covers genuine boundary-criteria renegotiation — this reading dissolves the ethnic boundary rather than sanctifying it — not identity framing used as cover for extraction. All three tracked series share one time grid (t = 0, 6, 12, 18, 24, 30); no metric is sampled on a private schedule.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is faithful stewardship: keep the whole book, follow the conscience the later prophets sharpened, absorb the criticism that comes with carrying a hard text. From the enclave-dissenter seat the same arrangement is an abandoned promise: the tradition announces it has moved on while leaving pockets where nothing moved, and the announcement itself makes their testimony harder to hear. From the literalist-clergy seat it is a career gate; from the Jewish-communal seat (covenant strand) it is the theft of election dressed as moral progress. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real positions: congregants and the intermarried receive admission and a usable ethic and bear almost nothing (d near the beneficiary end); the teaching authorities both administer the arrangement and collect its standing (damped effective extraction despite agenda-setting power). Victim declarations: enclave dissenters are trapped by kinship and sit nearest the full-target end; literalist clergy are identity-locked, which amplifies their effective targeting beyond what their nominal mobility alone would suggest; Jewish communities are organized with constrained exit, and their position is strand-dependent — near-symmetric under the prophetic-universalism strand, strongly targeted under the covenant-transfer strand — which the omega variables record rather than resolve. Suppression is declared but never scaled; extractiveness alone is scaled by directionality and by the arrangement's global scope, which modestly amplifies effective extraction by making verification of enclave practice harder. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the intended d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keep the text, lose the order — is still live and its resolution is precisely what the three readings dispute, so the arrangement is not yet a piton: enforcement still actively holds the middle against both the enclave right and the excisionist left, and the teaching offices still collect enough standing from mediating the hard text that they maintain it deliberately. The tangled_rope classification prevents two mislabels: reading the arrangement as pure coordination would hide the seats still paying (enclave minorities, transferred covenant, gated clergy); reading it as pure extraction would hide the genuine goods it delivers — intermarried members admitted, violence delegitimated, a canon kept whole without a war to show for it. The original mandate is resolved (no body may follow the directive), while the meta-mandate — administering the text's retirement — remains very much alive; that distinction is the story's center of gravity, and the R5 mismatch consumer should read status=contested against verdict=world_rearranges as a live arrangement, not a zombie.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superseding_agent_strand_ambiguity,
    'Which superseding agent governs this reading''s instantiation in a given community — the prophetic-universalist strain internal to Israel''s scriptures, or the Christian covenant that transfers Israel''s commission to the Church?',
    'Audit community teaching materials and interfaith stances to classify which strand each deploying body actually teaches; compare victim-set composition across strand-holding communities.',
    'Under the prophetic strand the Jewish-communal seat''s costs nearly vanish and the story trends toward low-extraction coordination; under the covenant strand that seat is strongly targeted and effective extraction rises materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superseding_agent_strand_ambiguity, conceptual, 'Strand ambiguity inside the supersessionist reading changes the beneficiary/victim map.').

omega_variable(
    historical_phase_moral_status,
    'Was the settlement-era herem itself, assessed by this reading''s own universalist lights, an obedience owed to a real command of its moment, or already a violation that the later prophets condemn?',
    'Source-critical dating of the Deuteronomistic strata against the prophetic critique literature (Hosea, Amos, Micah) to establish whether universalist condemnation predates, accompanies, or postdates the conquest material.',
    'Fixes where the moral supersession point sits on the time grid: if the command was condemned in its own era, early-interval values carry condemned-violence valence and the reading''s bounded-legitimacy claim weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_phase_moral_status, empirical, 'Dating the moral supersession point determines the sign of the historical phase.').

omega_variable(
    residual_enforcement_prevalence,
    'How much coercive enforcement of the old directive persists in enclave communities despite the dominant supersessionist teaching?',
    'Comparative survey of enclave discipline practice: shunning and expulsion rates for intermarriage and voiced dissent, against matched mainstream communities.',
    'Sizes the narrow victim set; near-zero prevalence would push the instantiation toward benign coordination with inertial retention, substantial prevalence would mean the authored extraction understates present harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_enforcement_prevalence, empirical, 'Prevalence of enclave enforcement sets the true width of the residual victim set.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of kernel herem_command_dt7; what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Compare the three stories'' beneficiary/victim sets and epsilon values: the durable-separation sibling widens victims to all cross-line members and indexes the arrangement as ongoing legitimate coordination; the allegorical-displacement sibling empties the ethnic victim set and bills the self instead. The disagreement lives in binding-temporal-scope and the referent of ''the nations.''',
    'If the kernel''s binding scope is timeless, this reading''s expiry axiom fails and the durable structure returns; if the referent is typological, the historical victim set dissolves and the arrangement migrates inward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one kernel, three readings, disagreement located in scope and referent.').

omega_variable(
    canon_retention_functionality,
    'Is retaining the directive in the closed canon functionally necessary for the communities'' scriptural integrity, or is it inertia that a cheaper settlement would serve?',
    'Compare cohesion and identity outcomes between communities that retain and teach the text under supersession, communities that quietly skip it, and the historical Marcionite excision experiment.',
    'If retention is functional, the coordination half of the tangled_rope verdict is genuine; if inertial, the theater_ratio is mostly dead weight and the arrangement drifts toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canon_retention_functionality, empirical, 'Whether canon retention earns its keep or persists as inertia.').

omega_variable(
    internalized_reading_discipline,
    'How much of the arrangement''s present suppression is internalized — members self-censoring literalist sympathy without any institutional act — rather than externally enforced?',
    'Contrast disciplinary cases with expressed-belief surveys across generations since the teaching consolidated; persistence of self-censorship after institutional relaxation marks internalization.',
    'If internalized, suppression would persist even if the teaching offices relaxed, meaning the authored suppression understates the arrangement''s grip and exit costs are higher than institutional structure alone shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_reading_discipline, empirical, 'Structural versus internalized share of the arrangement''s hold on reading choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_ctx_super_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(herem_ctx_super_tr_t6, herem_command_dt7__contextual_supersession_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(herem_ctx_super_tr_t12, herem_command_dt7__contextual_supersession_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(herem_ctx_super_tr_t18, herem_command_dt7__contextual_supersession_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(herem_ctx_super_tr_t24, herem_command_dt7__contextual_supersession_reading, theater_ratio, 24, 0.47).
narrative_ontology:measurement(herem_ctx_super_tr_t30, herem_command_dt7__contextual_supersession_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(herem_ctx_super_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(herem_ctx_super_be_t6, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(herem_ctx_super_be_t12, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(herem_ctx_super_be_t18, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 18, 0.44).
narrative_ontology:measurement(herem_ctx_super_be_t24, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(herem_ctx_super_be_t30, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 30, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(herem_ctx_super_su_t0, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(herem_ctx_super_su_t6, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(herem_ctx_super_su_t12, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(herem_ctx_super_su_t18, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(herem_ctx_super_su_t24, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(herem_ctx_super_su_t30, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% One kernel, three constraint stories. The colloquial label 'what Deuteronomy 7 commands' conflates three structurally distinct claims with different epsilon values: a timeless membership mandate (durable_separation_reading — high ongoing extraction, wide victim set), an internalized spiritual-warfare discipline (allegorical_displacement_reading — ethnic victims vanish, self-discipline billed), and a historically bounded directive under moral supersession (this story — residual extraction narrowed to enclave-held minorities and, on the covenant strand, Jewish communities). Each story carries its own epsilon, beneficiaries, and victims; family links run through network.affects_constraints. Upstream/downstream: the durable reading is the text's plainest historical enforcement shape; this reading and the allegorical reading are downstream defections from it, and this reading's expiry axiom is what the durable reading's timeless-binding axiom directly contradicts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
