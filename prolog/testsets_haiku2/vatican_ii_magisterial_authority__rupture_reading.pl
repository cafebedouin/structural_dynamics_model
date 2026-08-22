% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Rupture Reading: Magisterial Authority Reconstituted
 *   domain: ecclesiastical/doctrinal/institutional
 *
 * SUMMARY:
 *   Vatican II (1962-1965) was called by Pope John XXIII to update the Church
 *   for the modern world and has been the subject of interpretive contest
 *   ever since. The rupture reading claims the conciliar texts encode a
 *   fundamentally new ecclesiology incompatible with pre-conciliar teaching—a
 *   break with the past authorized by the Council itself. This reading
 *   legitimizes post-conciliar reforms (vernacular liturgy, religious freedom
 *   doctrine, ecumenical openness, catechetical revision) as mandated by the
 *   Council. The beneficiaries are reformist clergy and theologians who gain
 *   institutional authority to implement the new direction; the victims are
 *   traditionalist communities and defenders of pre-conciliar doctrine, who
 *   face institutional marginalization and suppression of their preferred
 *   liturgical and doctrinal forms. This is a tangled_rope: genuine
 *   coordination function (the Council solves the problem of institutional
 *   adaptation) AND asymmetric extraction (some get the authority to
 *   implement the new direction, others bear the cost of being declared
 *   obsolete). The constraint's persistence depends on active
 *   enforcement—marginalizing traditionalists, controlling catechetical
 *   content, limiting access to the Latin Mass—not merely on consensus.
 *
 * KEY AGENTS:
 *   - reformist_clergy_and_theologians: institutional beneficiaries — gain interpretive authority and implementation power from the rupture reading
 *   - progressive_episcopal_conferences: institutional agenda-setters — exercise discretion in implementing the new ecclesiology at the regional level
 *   - rome_magisterial_office: institutional agenda-setter/observer — the source of the authoritative texts but also the pivot point where rupture interpretation could be halted
 *   - traditionalist_communities: moderate-power victims — identity-locked victims subjected to institutional pressure to accept the rupture reading
 *   - pre_conciliar_doctrine_defenders: moderate-power victims — constrained victims facing professional marginalization for defending continuity
 *   - laity_experiencing_reform: powerless beneficiary/victims — benefit from accessibility and cultural openness but lose continuity and familiar form
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.52).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Rupture Reading: Magisterial Authority Reconstituted").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiastical/doctrinal/institutional").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '045378dc-1242-4ab3-a550-630113bceeed').
narrative_ontology:cs_kernel_codification('045378dc-1242-4ab3-a550-630113bceeed', formalized).
narrative_ontology:cs_authority_grounding('045378dc-1242-4ab3-a550-630113bceeed', lineage).
narrative_ontology:cs_interpretation_layer_present('045378dc-1242-4ab3-a550-630113bceeed').
narrative_ontology:cs_reading_relation('045378dc-1242-4ab3-a550-630113bceeed', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('045378dc-1242-4ab3-a550-630113bceeed', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('045378dc-1242-4ab3-a550-630113bceeed', foundational, vatican_ii_rupture_with_prior_magisterium).
narrative_ontology:cs_axiom_status(vatican_ii_rupture_with_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('045378dc-1242-4ab3-a550-630113bceeed', vatican_ii_rupture_with_prior_magisterium, deontological).
narrative_ontology:cs_axiom('045378dc-1242-4ab3-a550-630113bceeed', foundational, error_has_human_rights_doctrine_supersedes_prior_norm).
narrative_ontology:cs_axiom_status(error_has_human_rights_doctrine_supersedes_prior_norm, holdable).
narrative_ontology:cs_axiom_grounding('045378dc-1242-4ab3-a550-630113bceeed', error_has_human_rights_doctrine_supersedes_prior_norm, deontological).
narrative_ontology:cs_reference_frame('045378dc-1242-4ab3-a550-630113bceeed', pre_conciliar_magisterial_authority_regime).
narrative_ontology:cs_drift_state('045378dc-1242-4ab3-a550-630113bceeed', post_vatican_ii_implementation_era_contemporary, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('045378dc-1242-4ab3-a550-630113bceeed', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, reformist_clergy_and_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_episcopal_conferences).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_doctrine_defenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, laity_experiencing_reform).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, laity_experiencing_reform).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Vatican II texts as authorizing fundamental reimagining of Church structures, liturgy, and doctrine. Gain institutional legitimacy and implementation authority from the reading that frames conciliar texts as mandating experimentation and doctrinal development. Their authority rests on the rupture interpretation: the Council opened a new era and they are its legitimate executors.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, reformist_clergy_and_theologians, beneficiary,
    institutional, generational, constrained, global).

% Regional episcopal bodies that implement the rupture reading through liturgical reform, catechetical revision, and selective application of pre-conciliar norms. They exercise doctrinal and pastoral discretion on the assumption that Vatican II text is the legitimate authority and pre-conciliar positions are superseded. Their power to shape implementation depends on the rupture reading holding institutional authority.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_episcopal_conferences, agenda_setter,
    institutional, generational, mobile, regional).

% The central authority that issued Vatican II documents and continues to interpret them. Sits in structural ambiguity: the rupture reading claims Vatican II texts authorize the new direction, but Rome also inherits pre-conciliar teaching and must adjudicate conflicts. The reading's persistence requires Rome to endorse or permit the rupture interpretation; Rome's hesitation or correction would undermine it. Authority over the constraint's own interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, rome_magisterial_office, agenda_setter,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, rome_magisterial_office, observer).

% Religious communities and laity committed to pre-conciliar liturgy, theology, and ecclesiology. Bear the cost of institutional marginalization, reduced access to Latin Mass, and doctrinal correction when they defend pre-conciliar positions. Their identity is fused with the pre-conciliar form; exit means renouncing not just a practice but a self-understanding of what the Church is. Subjected to enforcement pressure to accept the rupture reading as binding.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_communities, payer,
    moderate, generational, identity_locked, global).

% Theologians, canonists, and clergy who maintain that Vatican II affirmed rather than superseded prior magisterial positions (religious error has no rights, liturgical Latin's place, episcopal authority limits). Face institutional pressure—removal from teaching positions, exclusion from reform bodies, characterization as 'not understanding the Council'—when they defend continuity against the rupture reading's claims. Their exit is constrained by professional investment and ecclesiastical identity.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_doctrine_defenders, payer,
    moderate, biographical, constrained, global).

% Ordinary Catholics whose parish experience was transformed by liturgical reform and catechetical revision premised on the rupture reading. Many benefit from vernacular liturgy, accessible theology, and openness to modern culture. Others experience alienation from the familiar form and sense a loss of continuity. No institutional voice in how the constraint is interpreted; they experience its effects in lived practice.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, laity_experiencing_reform, beneficiary,
    powerless, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, laity_experiencing_reform, payer).

% Non-Catholic Christian traditions and observers who witness the Vatican II rupture reading as enabling or hindering ecumenical progress. Protestant and Orthodox interlocutors may welcome the opening of new ecclesiological space or worry that the Church's internal incoherence makes dialogue unstable. No direct role in the constraint but exposed to its consequences for Christian unity discourse.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, ecumenical_partners, observer,
    institutional, generational, analytical, global).

% The authoritative documents (Lumen Gentium, Sacrosanctum Concilium, Dignitatis Humanae, etc.) whose interpretation is contested. Not an agent but a non-agent entity kept for narrative completeness: the text's authority is what is fought over. The rupture reading benefits from framing the text as unambiguously mandating the new direction.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_conciliar_text, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_conciliar_text).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__rupture_reading, reformist_clergy_and_theologians).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the institutional problem of how the post-World War II Church adapts to modern pluralism, religious freedom discourse, and ecumenical engagement. Centralizes the solution via conciliar authority: the Council speaks for the entire Church and its word is binding on all levels. The rupture reading solves the coordination problem by making the Council's interpretation clear and actionable—new ecclesiology is not optional, it is the legitimate direction.
% TRANSFER_FUNCTION: Transfers magisterial authority from pre-conciliar doctrinal norms and liturgical forms to conciliar texts read as opening a new era. Moves interpretive power from centralized Rome to regional episcopal conferences (subsidiarity). Moves liturgical practice from Latin, fixed forms, and strict rubrical control to vernacular, experiential, and pastorally adaptive practice. Moves religious personhood doctrine from 'error has no rights' to 'religious freedom is a human right.' Winners: reformist implementers gain institutional mandate; losers: traditionalist communities lose institutional support and face marginalization.
% ABSENT_VOICES: The pre-conciliar magisterium itself—Pius XII, Pius XI—cannot defend its own positions once declared superseded; their voice is represented only through traditionalist interpreters who are marginalized by the rupture reading. The laity who prefer the old form have no institutional standing in interpretation; their subsequent experience of liturgical alienation is not registered as a cost in conciliar calculations. The Orthodox and Protestant observers at the Council left before the rupture interpretation solidified; their concerns about ecclesiological incoherence and the risks of internal contradiction are retrospectively external to the constraint.
% DISAPPEARANCE_RATIONALE: If the rupture reading were authoritatively rejected and replaced by the continuity reading, the entire post-1965 implementation architecture would require reconstruction. The Latin Mass would be restored to regular availability as a legitimate norm, not a tolerated exception. Catechetical materials would revert to pre-conciliar content or be heavily revised to emphasize continuity with prior magisterium. Seminary formation in ecclesiology and moral theology would change direction. Ecumenical postures would shift from openness to caution about non-Catholic churches. Married priests and women's ordination conversations would halt. The institutional Church would reorganize around the continuity interpretation, and the trajectory of reform would stabilize or reverse. The constraint's disappearance means the lived reality of Catholicism for 1.3 billion believers would be fundamentally different in worship, doctrine, and institutional self-understanding.
% FOUNDING_PROBLEM: The Church faced institutional crisis post-WWII: institutional credibility eroded by wartime silence and failure to resist totalitarianism; young clergy and theologians demanded engagement with modernity; Protestant ecumenism and new religious freedom discourse challenged traditional positions; liturgical alienation among educated laity rising sharply; the Second Vatican Council was called specifically to address this crisis of relevance and institutional adaptation to the modern world.
% FOUNDING_PROBLEM_CORROBORATION: The reformist reading (rupture) attests the founding problem is solved by Vatican II's explicit authorization of change—the Council solved the crisis by legitimizing modernization, and the constraint ensures implementation stays true to that mandate. The continuity reading attests the founding problem was misdiagnosed—the real problem was internal confusion about whether doctrine could be applied to new circumstances, not doctrine itself, and Vatican II solved it by clarifying continuity while permitting pastoral application. Historians outside the directly benefiting camp (John W. O'Malley's institutional history, Agostino Marchetto's defense of conciliar intentionality) acknowledge the Council addressed institutional adaptation but differ sharply on whether implementation reflects or exceeds the texts. No unanimous external corroboration exists for the rupture interpretation of the founding problem; the founding problem itself is constitutively disputed between the readings.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the constraint's operation as a magisterial authority shift. The rupture reading extracts from traditionalist seats the right to defend pre-conciliar doctrine as Church teaching; it extracts from pre-conciliar-sympathetic regions the right to set their own liturgical pace. Extraction is not confiscatory (the Church does not physically imprison traditionalists) but institutional: access to preferred forms is withdrawn, doctrinal positions are declared erroneous, implementation is mandated rather than negotiated. Suppression (0.52) is moderate because traditionalism persists despite pressure—it is not fully suppressed but constrained, limited to secondary status. Theater (0.41) indicates that enforcement includes performative work: declarations of conciliar mandate, celebration of 'the spirit of Vatican II,' catechetical emphasis on rupture as progress, liturgical choreography that privileges the new form. But the theater is not complete performance—actual reforms occurred, actual authority shifted, actual costs were borne. The measurement series shows extractiveness and suppression rising through the 1965-1975 decade (post-conciliar period when implementation accelerated), plateauing by 1985-2000 (institutionalization complete, traditionalism confined but stable). The accessibility_collapse (0.71) reflects how completely the rupture reading forecloses the continuity alternative once institutional machinery accepts it—continuity becomes a marginal position requiring substantial counter-institutional work to maintain.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist seat, this is a genuine coordination solution: the Church adapts to modernity and solves an institutional crisis. From the traditionalist seat, this is extraction: magisterial authority is used to suppress what was once authoritative teaching and enforce a reading that has no clear textual mandate. Rome sits in structural ambiguity—as the agenda-setter that issued the Council's texts, it can claim to be impartially enforcing them; but as the institution that decides how to interpret them, it is the seat where the rupture/continuity choice is made. The engine should compute different types across seats: reformists see a rope (genuine coordination they initiated); traditionalists see a snare (extraction enforced by institutional power); Rome sees a constrained tangled_rope (coordination function real, but asymmetric extraction happening and unresolvable).
 *
 * DIRECTIONALITY LOGIC:
 *   Reformists are low-d beneficiaries: the constraint subsidizes their authority, aligns with their theological commitments, and gives them power to implement their vision. Traditionalists are high-d targets: the constraint extracts from them the institutional standing of pre-conciliar doctrine, restricts their preferred liturgical practice, and makes their identity a liability. Pre-conciliar doctrine defenders sit between (moderate-d payers): their exit is constrained (professional investment in the Church) but they have some leverage (they can publish, petition Rome, form alternative communities). Laity are symmetric near d=0.5: genuine benefit from accessibility and cultural relevance, genuine cost from loss of continuity and alienation for those attached to old forms. Rome is an outlier: agenda-setter with trapped exit (cannot step outside the magisterial role without dissolving authority) and institutionalized time horizon (the papacy is a 2000-year commitment). Directionality overrides are not needed—the structural derivation tracks the actual situation.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading averts false mandatrophy by claiming Vatican II texts unambiguously authorize the new direction. But the mandate is contested: continuity readers argue the texts affirm development, not rupture. The constraint's persistence depends on Rome accepting the rupture interpretation and progressively institutionalizing it through implementation decisions (Paul VI's post-conciliar commissions, JP2's catechesis, Francis's symbolic moves). If Rome were to declare that Vatican II was misinterpreted—that it authorized development, not rupture—the entire post-1965 implementation would be delegitimized, traditionalist positions would be restored to orthodoxy, and the constraint would collapse. The risk of mandatrophy is real: the longer the constraint persists with the founding problem (institutional modernization) solved, the more the rupture reading looks like institutional authority being used to enforce preferred theology rather than necessary adaptation. The four omegas all point to this: if textual mandate is unclear, if implementation exceeded authorization, if identity-locking is structural, or if the kernel intentionally permits pluralism, then the constraint is extractive use of magisterial power, not coordination solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_interpretive_ambiguity,
    'Does the Vatican II text itself unambiguously authorize the rupture interpretation, or does the text permit both continuity and rupture readings as internally coherent?',
    'Systematic exegesis of conciliar documents by scholars explicitly tasked with disambiguating hermeneutical intention rather than defending one reading. Cross-reference Council periti (theological advisors) notes, voting patterns on specific formulations, and documented conciliar debate on doctrinal change vs. development. Compare the text''s language on religious freedom, liturgy, and ecclesiology against its own stated principle of hermeneutical continuity.',
    'If the text unambiguously authorizes rupture, the continuity reading is overridden and the rupture reading''s extraction of magisterial authority from pre-conciliar sources is legitimate. If the text permits both, then the rupture reading rests on supplementary institutional decisions (Pope Paul VI''s implementation choices, episcopal conference interpretations) rather than textual mandate, shifting the constraint''s legitimacy source from conciliar authority to post-conciliar administrative authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_vs_rupture_interpretive_ambiguity, conceptual, 'Whether Vatican II texts textually determine the rupture interpretation or permit alternative coherent readings.').

omega_variable(
    magisterial_self_limitation_doctrine,
    'Can the magisterium bind itself to a rupture with its own prior teaching, or does magisterial authority include the power to reverse or reabsorb prior doctrine into a larger continuity frame?',
    'Theological analysis of magisterial authority as a property: is it unlimited (the magisterium can declare anything doctrinal), or does its legitimacy rest on consistency with prior teaching, making rupture a use of authority that undermines the authority''s own epistemic grounding? This is a conceptual/preference distinction, not empirical.',
    'If magisterial authority includes unlimited power to declare rupture, the constraint stands on rock. If magisterial authority is grounded in consistency with tradition, then the rupture reading weakens magisterial authority by claiming its own texts supersede what it once authoritatively taught—undercutting the very legitimacy the reading needs to enforce the rupture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_self_limitation_doctrine, conceptual, 'Whether magisterial self-contradiction can be legitimate doctrine or whether it undermines the authority doing it.').

omega_variable(
    implementation_cost_vs_textual_authorization,
    'To what degree is the post-1965 implementation of Vatican II (vernacular liturgy, catechetical revision, ecumenical openness) textually mandated by the conciliar documents versus administratively initiated by Rome and the episcopal conferences under the umbrella of the rupture reading?',
    'Comparative textual analysis: what does Sacrosanctum Concilium actually require (Latin, fixed liturgy, experimental vernacular, or Latin-vernacular integration)? What did Pope Paul VI''s motu proprio Ecclesiae Sanctae actually authorize? When did experimentation exceed textual mandate? Document the gap between conciliar text and implementation history. Interview historical actors about authorization reasoning.',
    'If the rupture reading merely summarizes textual mandates, the constraint is legitimate textual enforcement. If implementation substantially exceeded textual authorization, then the rupture reading is doing post hoc work to justify administrative decisions, and the constraint''s extraction is less from conciliar authority than from institutional assertion dressed in conciliar language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_cost_vs_textual_authorization, empirical, 'Whether post-Vatican II implementation is textually mandated or administratively exceeded the text and claimed textual backing.').

omega_variable(
    identity_locked_exit_mechanism,
    'For traditionalist communities, is the identity-locked exit option a structural feature of the constraint (the constraint makes exit unthinkable by fusing self with pre-conciliar form), or a psychological/cultural feature of the communities themselves that exists independently?',
    'Counterfactual: if the institutional Church had explicitly affirmed that pre-conciliar practice remains licit and honored, would traditionalist communities'' sense of identity-fusion change? Post-exit trajectory studies: when traditionalists do exit (form independent communities, join FSSP, leave priesthood), does the identity-fusion persist or dissolve? This tests whether suppression is structural (the constraint creates it) or internalized (the communities carry it).',
    'If identity-locking is structural—the constraint creates it by marginalizing the form, making it impossible to live within the Church while maintaining the identity—then the suppression is higher than authored and the extraction is more severe. If it is pre-existing, the constraint rides on it but does not fully create it, and the beneficiaries'' power over traditionalists is constrained by the willingness of the victims to internally accept the marginalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Whether the identity-locked exit is created by the constraint or pre-exists it in the communities themselves.').

omega_variable(
    kernel_reading_contest_source,
    'This constraint instantiates the rupture reading of the vatican_ii_magisterial_authority kernel. Why do three readings (rupture, continuity, composite_overdetermination) coexist with equal legitimacy-claim in the theological literature and institutional practice?',
    'Meta-analysis of the conciliar text''s linguistic structure: does it intentionally encode ambiguity to permit competing implementations, or is the ambiguity an accident of rapid drafting and compromise? Interview conciliar historians on intentionality. Examine whether the three readings correspond to systematic theological schools (Ressourcement vs. New Theology vs. Administrative Pragmatism) or emerge from political factions (conservative vs. reformist episcopates).',
    'If the text intentionally encodes pluralism, the constraint is not the rupture reading enforcing a clear mandate but rather one faction using the ambiguous text to claim authority and suppress the other readings'' implementers. If ambiguity is accidental, the constraint emerges from bureaucratic elaboration (Rome and conferences filling gaps) claiming textual backing it does not have. Either way, the legitimacy of the rupture reading rests more on institutional power than on unambiguous textual authorization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_source, conceptual, 'Why the vatican_ii_magisterial_authority kernel admits three coequal interpretive readings instead of resolving to one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(vati_tr_t0, projected).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(vati_be_t0, projected).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(vati_su_t0, projected).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__rupture_reading, 0.18).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The vatican_ii_magisterial_authority kernel decomposes into three structurally distinct constraints, each representing a different reading of the Council's meaning and authority. The rupture reading (this constraint) is one pole of a live interpretive contest. The continuity reading and composite_overdetermination reading are sibling constraints instantiating alternative readings of the same kernel. All three readings agree the Vatican II texts are authoritative but differ fundamentally on what they authorize. The rupture reading's extractiveness and suppression of traditionalist positions depends partly on excluding the institutional legitimacy of the continuity reading. Each sibling constraint must be evaluated independently; the engine's per-seat classification should show how reformists, traditionalists, and Rome compute different types depending on which reading they hold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
