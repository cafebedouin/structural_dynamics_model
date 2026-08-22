% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: The 'Spirit of the Council' Authorization Structure — Progressive Rupture Reading
 *   domain: ecclesial/institutional/hermeneutic
 *
 * SUMMARY:
 *   The kernel is the doctrinal authority of Vatican II; this file
 *   instantiates one reading of it — the progressive rupture reading, which
 *   holds the Council as a necessary break with pre-conciliar rigidity and
 *   treats the 'spirit of the Council' as authorizing reform beyond the
 *   documents' textual limits. As a standing arrangement, that reading
 *   operates as an authorization structure: whoever adjudicates conciliar
 *   intent may steer adaptation without reopening the negotiated texts, while
 *   attachment to the letter is coded as rigidity. Three sibling readings
 *   (continuity, traditionalist rupture, composite overdetermination) are
 *   separate constraints with their own ε values and are linked only through
 *   the kernel family. The ε authored here is for the standing arrangement —
 *   the spirit-primacy regime as it has actually operated from 1965 to the
 *   present — assessed by this reading's own lights: the reading endorses the
 *   reform direction but authors honestly the costs the regime imposes on
 *   traditionalist communities, text-bound interpreters, and unauthorized
 *   reformers, and the way the spirit-gate concentrates discretionary
 *   authority in the hierarchical center. Claim and metrics are independent:
 *   the tangled_rope claim reflects the structure (real implementation
 *   coordination plus asymmetric gate extraction plus active enforcement),
 *   not the reading's self-understanding.
 *
 * KEY AGENTS:
 *   - roman_magisterium: agenda-setter (institutional/arbitrage) — adjudicates conciliar intent, steers implementation, can reframe the hermeneutic between pontificates
 *   - progressive_theological_establishment: dual-positioned beneficiary/payer (organized/identity_locked) — staffs the implementation machinery; authorized when aligned, censured when not
 *   - traditionalist_communities: primary payer (moderate/identity_locked) — attachment recoded as rigidity; survives in tolerated enclaves
 *   - text_bound_theologians: payer (moderate/constrained) — hold the letter binds; subordinated in spirit-primacy venues
 *   - attached_preconciliar_laity: payer (powerless/trapped) — absorbed implementation without a consultative seat
 *   - continuity_party: payer (powerful/identity_locked) — held the papacy 2005-2013; payer exposure with institutional shielding
 *   - national_bishops_conferences: beneficiary (institutional/constrained) — derivative latitude, withdrawable by decree
 *   - ecumenical_partners: excluded (moderate/mobile) — repriced without consultation when the hermeneutic shifts
 *   - council_historiography: analytical observer — established the documents' negotiated ambiguity from the archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.62).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.58).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "The 'Spirit of the Council' Authorization Structure — Progressive Rupture Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesial/institutional/hermeneutic").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, '1a332157-792d-4c8d-8cd5-362e40948ca5').
narrative_ontology:cs_kernel_codification('1a332157-792d-4c8d-8cd5-362e40948ca5', fixed_text).
narrative_ontology:cs_authority_grounding('1a332157-792d-4c8d-8cd5-362e40948ca5', lineage).
narrative_ontology:cs_interpretation_layer_present('1a332157-792d-4c8d-8cd5-362e40948ca5').
narrative_ontology:cs_reading_relation('1a332157-792d-4c8d-8cd5-362e40948ca5', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('1a332157-792d-4c8d-8cd5-362e40948ca5', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_axiom('1a332157-792d-4c8d-8cd5-362e40948ca5', foundational, conciliar_spirit_surpasses_conciliar_letter).
narrative_ontology:cs_axiom_status(conciliar_spirit_surpasses_conciliar_letter, holdable).
narrative_ontology:cs_axiom_grounding('1a332157-792d-4c8d-8cd5-362e40948ca5', conciliar_spirit_surpasses_conciliar_letter, theological).
narrative_ontology:cs_axiom('1a332157-792d-4c8d-8cd5-362e40948ca5', foundational, doctrinal_reversal_under_changed_conditions_legitimate).
narrative_ontology:cs_axiom_status(doctrinal_reversal_under_changed_conditions_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('1a332157-792d-4c8d-8cd5-362e40948ca5', doctrinal_reversal_under_changed_conditions_legitimate, empirically_contingent).
narrative_ontology:cs_axiom('1a332157-792d-4c8d-8cd5-362e40948ca5', secondary, postconciliar_implementation_authentic_realization).
narrative_ontology:cs_axiom_status(postconciliar_implementation_authentic_realization, holdable).
narrative_ontology:cs_axiom_grounding('1a332157-792d-4c8d-8cd5-362e40948ca5', postconciliar_implementation_authentic_realization, conventional).
narrative_ontology:cs_reference_frame('1a332157-792d-4c8d-8cd5-362e40948ca5', spirit_of_council_primacy).
narrative_ontology:cs_drift_state('1a332157-792d-4c8d-8cd5-362e40948ca5', synodality_era_contemporary, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('1a332157-792d-4c8d-8cd5-362e40948ca5', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, roman_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theological_establishment).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, national_bishops_conferences).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, text_bound_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, attached_preconciliar_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, continuity_party).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theological_establishment).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, living_magisterium_precedence_over_conciliar_letter).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, conciliar_event_hermeneutics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The pope, the Roman Curia, and the doctrinal congregation define what the Council's intent requires. They approve liturgical forms, adjudicate theological work, and steer how the reforms reach dioceses and seminaries. Between pontificates they can reframe the governing hermeneutic — the 2005 address restoring continuity language and the 2021 restrictions on the pre-conciliar liturgy were both exercises of the same office — because the authority to say what the Council means is the office's own operating power.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, roman_magisterium, agenda_setter,
    institutional, generational, arbitrage, global).

% Professors, periti, liturgical commissions, and catechetical bodies whose post-conciliar work the spirit-language authorizes. They staff the machinery that turns conciliar intent into curricula, rites, and documents, and their reading of the Council is the one the center approves. When their conclusions run past what the center will carry, they lose mandata, chairs, or standing — the 1979 and 1984-86 cases are the reference points. Their professional lives are built on the Council as an opening; abandoning that frame would unravel their own work.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theological_establishment, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theological_establishment, payer).

% Orders, fraternities, and lay networks formed by pre-conciliar liturgy and doctrine. Implementation restricted their rites and recoded their attachment as a mentality the Council had superseded. They survive through tolerated enclaves and their own seminaries and schools, under periodic restriction (1988, 2021) and periodic tolerance (2007). Leaving the Church would forfeit the recognition they exist to claim — that they are its faithful members — so they remain inside and absorb the marginalization.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_communities, payer,
    moderate, generational, identity_locked, global).

% Scholars who hold that the Council's documents themselves bind — that the negotiated compromises are part of what the Council teaches. In venues where intent outranks letter, their work is received as insufficiently responsive to what the Council 'really' meant. They publish and teach in academies and journals outside the authorized circuit, at the cost of influence over implementation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, text_bound_theologians, payer,
    moderate, biographical, constrained, global).

% Parishioners whose worship and catechesis were the pre-conciliar forms. Implementation changed both within a few years, without a consultative seat for them. Their paths were assimilation to the reformed forms, movement to traditionalist enclaves, or drift out of practice — each carrying costs in identity, community, and access to the sacraments.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, attached_preconciliar_laity, payer,
    powerless, generational, trapped, global).

% Cardinals, theologians, and institutions holding that the Council developed tradition without reversing it. The position held the papacy from 2005 to 2013 and used that tenure to restore textual accountability — the hermeneutic address of 2005 and the 2007 liberalization of the pre-conciliar liturgy. Within spirit-primacy venues their reading is coded as insufficiently responsive to the Council's real intent; their institutional strength protects them from the censure other dissenting seats absorb.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, continuity_party, payer,
    powerful, generational, identity_locked, global).

% Episcopal conferences received post-conciliar latitude — collegiality, vernacular liturgy under their translation authority, local adaptation of implementation. The latitude is derivative: it extends as far as the center's reading of conciliar intent allows, and the 2021 restrictions showed it can be curtailed by decree without their consent.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, national_bishops_conferences, beneficiary,
    institutional, generational, constrained, continental).

% Anglican, Orthodox, Lutheran, and Reformed bodies in dialogue with Rome. Their agreed texts price the Church's doctrinal development as they understand it; when the governing hermeneutic shifts, decades of agreed wording are repriced without their having any voice in the adjudication that reprices them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_partners, excluded,
    moderate, generational, mobile, global).

% Academic historians of the Council — the Bologna school and its critics — working from diaries, archives, and the acta. They established that the documents were negotiated compromises whose ambiguities were deliberate, and they supply the evidentiary base every reading draws on. They hold no stake in which hermeneutic governs.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, council_historiography, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, roman_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gave a global church a workable way to act on a deliberately ambiguous compromise council: a shared authorization to implement the Council's intent without reopening the negotiated texts, and a single lever — the center's adjudication of that intent — by which adaptation could be steered coherently across thousands of dioceses.
% TRANSFER_FUNCTION: Moves interpretive authority from the conciliar letter to whoever adjudicates conciliar intent; moves legitimacy from inherited pre-conciliar forms to center-approved reform; moves doctrinal labor from text-bound exegesis to spirit-discernment under hierarchical approval. Net flow: from traditionalist communities, text-bound scholars, and laity attached to inherited forms, to the hierarchical center and the post-conciliar establishment that staffs it.
% ABSENT_VOICES: The laity who absorbed implementation had no seat; ecumenical partners whose agreements the development reprices have no voice in the adjudication; the pre-conciliar tradition's own witnesses appear only through marginalized traditionalist intermediaries; and the Council's minority — the bishops who fought the documents' open texture — survives only in archives the rival readings cite against each other.
% DISAPPEARANCE_RATIONALE: If the spirit-primacy regime vanished overnight, every post-conciliar reform would lose its authorization at once: the liturgical forms, the ecumenical openings, the religious-liberty teaching, and the collegial structures would all face an immediate legitimacy hearing against the letter, the center would have to choose between the continuity and traditionalist readings, and the theological establishment built on the Council-as-opening would lose its charter. The post-conciliar Church is organized around this regime; its disappearance is not absorbable.
% FOUNDING_PROBLEM: Two problems fused at the Council's close: the pre-conciliar system's rigidity — a Syllabus-era posture that had frozen doctrinal development and set the Church against the modern political order — needed breaking; and the Council's own documents, negotiated compromises whose ambiguities were the price of near-unanimity, needed an implementation mechanism that would not reopen the fights they had settled.
% FOUNDING_PROBLEM_CORROBORATION: Council historiography — outside every confessional benefiting seat — corroborates the second half: the documents were deliberate compromises, and the diaries and acta are public. Benedict XVI's 2005 hermeneutic address, from outside the progressive benefiting set, corroborates the first half (the rigidity was real) while disputing the spirit-beyond-text solution. Traditionalist communities, also outside the benefiting set, dispute the first half's framing entirely, holding that rigidity was fidelity. No source outside the dispute denies that an implementation problem existed; what is contested is whether the spirit-gate is its solution or a new problem laid over the old one.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.62 at interval end: the spirit-gate decouples operative authority from the letter, and its yields (implementation control, censure power, liturgical steering) accrue to the center while the costs land on those attached to inherited forms and those whose reform outruns authorization. Suppression 0.58: enforcement is canonical and institutional (censures, restricted rites, withdrawn mandates) reinforced by internalized obedience formation — structural in the larger share, internalized in the smaller; the split is carried by omega suppression_structural_vs_internalized. Theater 0.42: the spirit-language does real hermeneutic work, but a growing share of its use is ritual invocation — 'the spirit of Vatican II' as an authorization token detached from determinate content, especially where synodal consultation arrives pre-framed. All three series run on one shared time grid (every metric authored at every point 0-60). The trajectories are pendular, not monotonic: enforcement and extraction peak in the implementation-and-censure era (t=10-20, the old-rite restrictions, the 1979 and 1984-86 censures, the 1988 excommunications), relax under the continuity counteroffensive (t=40, the 2005 hermeneutic address and 2007 liturgical liberalization), and re-ratchet in the synodality era (t=60, the 2021 restrictions and the revival of spirit-language for ongoing reform). The cycle is driven by which sibling reading controls the center — a side effect of contested control, not itself the extraction mechanism; the diagnostic asymmetry is that the floor never returns to t=0 levels, which omega extraction_ratchet_asymmetry carries. Base properties are authored at the end state (t=60).
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the center, the arrangement is the Council faithfully implemented: the gate is how a global church acts on a negotiated text without dissolving. From the traditionalist and text-bound seats, the same structure is delegitimation of inherited fidelity — the letter they hold binding is ruled insufficient by an authority no text names. From the progressive establishment's seat it is both at once: authorization when aligned (the post-conciliar decades built their institutions on it) and gate when not (the censure record). The continuity party experienced the gate from the payer side while holding the papacy — payer exposure with institutional shielding, which is why its computed extraction should sit below the other payer seats despite identical victim status. Same-level divergence: text-bound theologians and the progressive establishment occupy the same nominal professional position, but authorization alignment and identity fusion give them different exits — the establishment cannot abandon the Council-as-opening without unraveling its life's work, while the text-bound retain an academy outside the authorized channels.
 *
 * DIRECTIONALITY LOGIC:
 *   The center is declared beneficiary and agenda-setter: it collects the gate's discretionary yield, so its derived directionality sits near the beneficiary end. The progressive establishment is declared beneficiary, but its secondary payer exposure (censure when unauthorized) is not captured by a beneficiary declaration alone — overridden to d=0.38 at the organized atom. Traditionalist communities, text-bound theologians, and attached laity derive high d from victim declarations, differentiated by exit: identity_locked traditionalists and trapped laity sit nearest the full-target end, constrained text-bound theologians slightly less. The continuity party is a victim by declaration but its papal tenure shields it — overridden to d=0.6 at the powerful atom. Bishops' conferences are beneficiaries with derivative, withdrawable latitude — moderately low d. Ecumenical partners are excluded rather than coordinated; their exposure is real but external to the adjudication. Council historiography is analytical and collects nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two fused halves: breaking pre-conciliar rigidity, and implementing an ambiguously negotiated council. The first is substantially accomplished — the Syllabus-era posture is gone and no reading in the kernel proposes restoring it; the second remains genuinely live, since every hermeneutic dispute is an implementation dispute. Classifying this as tangled_rope rather than snare preserves the first half's achievement from extraction-mislabeling; refusing the rope classification preserves the second half's asymmetry from coordination-mislabeling. The R5 mismatch check reads status=contested against verdict=world_rearranges: no zombie flag fires, correctly — the arrangement's world-rearranging character is real even though the parties dispute whether its founding problem persists. The mandatrophy risk sits elsewhere: if the first half's accomplishment is allowed to launder the second half's gate — if breaking rigidity once justifies the spirit-monopoly forever — the arrangement drifts toward extraction with a coordination cover story, which the ratchet omega watches.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the vatican_ii_doctrinal_authority kernel — the rupture_progressive_reading. What would the sibling readings change structurally if they governed instead?',
    'Cross-reading comparison within the constraint family: author and compile the continuity_reading and rupture_traditionalist_reading stories and compare ε, beneficiary/victim sets, and computed types across the kernel.',
    'Under the continuity_reading, doctrinal change is explication rather than reversal — ε on doctrinal change drops, the victim set shrinks to reformers denied further development, and the spirit-gate loses its justification. Under the rupture_traditionalist_reading, the same arrangement keeps high ε but is judged illegitimate outright — the victim becomes the tradition itself and enforcement becomes usurpation. The disagreement is located in the novelty status of Dignitatis Humanae (reversal vs. explication) and the binding force of the conciliar letter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story instantiates one of four readings of the Vatican II doctrinal-authority kernel.').

omega_variable(
    spirit_gate_separability,
    'Is the spirit-beyond-text authorization functionally separable from the center''s monopoly on adjudicating what the spirit requires?',
    'Comparative analysis of episodes where reform outran the center''s gate — the Dutch Catechism (1966), liberation theology (1970s-80s), the German Synodal Way (2019-): did the authorization function survive when the gate resisted, and at what cost to coherence?',
    'If separable, the arrangement''s extraction is substantially the center''s gatekeeping riding on a real hermeneutic need — a removable overlay. If inseparable, part of the measured extraction is the unavoidable price of any workable implementation mechanism for a negotiated compromise council, and the tangled_rope classification is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_gate_separability, conceptual, 'Whether the coordination and gatekeeping components of the spirit hermeneutic are structurally separable.').

omega_variable(
    dh_reversal_or_explication,
    'Is Dignitatis Humanae a reversal of the Syllabus of Errors'' teaching on religious toleration, or an explication of principles implicit in prior teaching under changed historical conditions?',
    'Doctrinal-historical analysis: Murray''s own development account, the Syllabus''s actual condemnations, and the internal debates of the Council''s doctrinal commission — assessed for whether the proposition content changed or only its historical application.',
    'This is the structural hinge between this reading and the continuity_reading. If reversal, this reading''s foundational axiom holds and the continuity reading cannot be maintained within a single framework. If explication, this reading''s ε on doctrinal change drops sharply and its spirit-beyond-text authorization loses its strongest exhibit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dh_reversal_or_explication, conceptual, 'The located disagreement between the progressive and continuity readings of the Council.').

omega_variable(
    extraction_ratchet_asymmetry,
    'Why does the arrangement''s extraction floor never return to its 1965 level after continuity counteroffensives (2005-2013) — is the ratchet structural (implementation creates irreversible facts) or contingent (personnel and papal style)?',
    'Counterfactual analysis of the Summorum Pontificum window: which post-conciliar structures proved irreversible when the center favored textual accountability, and did the spirit-language''s institutional carriers (faculties, liturgical bodies, catechetical apparatus) retain their positions?',
    'If structural, the pendulum reading of the measurements is wrong — the arrangement is a ratchet whose enforcement intensity oscillates while its extractive base accumulates, and long-run classification drifts toward snare. If contingent, the pendulum model holds and the arrangement remains a contested tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_ratchet_asymmetry, empirical, 'Whether the post-conciliar extraction trajectory is a ratchet or a pendulum.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the arrangement''s hold on clergy and laity is structural (canonical machinery, censure, liturgical restriction) versus internalized (formation that fuses ecclesial identity with the center''s spirit-discernment, so that dissent is felt as infidelity)?',
    'Post-exit trajectory: communities that left the arrangement''s jurisdiction (the 1988 excommunication episode and its later partial resolution) — did their attachment to the Council as organizing event persist after enforcement was lifted, indicating internalized fusion, or decay with the enforcement?',
    'If largely internalized, the arrangement''s effective suppression exceeds the canonical measure and persists where enforcement relaxes — raising the true suppression above the authored 0.58. If largely structural, enforcement relaxations (2007-2021) genuinely lower suppression and the pendulum is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized components of ecclesial suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 50, 0.34).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the doctrinal authority of Vatican II' covers four structurally distinct claims — four readings of one kernel — and per the ε-invariance principle each is authored as its own constraint with its own ε, beneficiaries, and victims. This file is the progressive rupture reading. The upstream member by empirical confidence is the composite_overdetermination_reading (the archival demonstration that the Council bundled several distinct changes is the least contested claim in the family); the continuity_reading and this reading are downstream competitors over the same documents' novelty status, and the traditionalist reading is downstream of both with an inverted valence. Family members are linked so legitimacy shifts and contamination propagate across the kernel instead of being absorbed as measurement noise inside any single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, organized, 0.38).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
