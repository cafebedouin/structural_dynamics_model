% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Magisterial Guardianship of the Deposit of Faith (Tradition-and-Scripture Reading)
 *   domain: theology/religious/history_of_christianity
 *
 * SUMMARY:
 *   Within the tradition-and-scripture reading of Christian authority,
 *   Scripture possesses authoritative meaning only as interpreted through
 *   Sacred Tradition, and the Magisterium — the pope and bishops in apostolic
 *   succession — guards the deposit of faith and adjudicates its meaning.
 *   Doctrine is received by the baptized rather than adjudicated by them;
 *   sacramental grace is conveyed through clergy-reserved sacraments; dissent
 *   from defined teaching draws discipline escalating to excommunication. The
 *   arrangement delivers a genuine coordination good — doctrinal unity and
 *   two-millennia continuity for a global communion — while concentrating
 *   interpretive authority and sacramental mediation in an office that
 *   collects deference, obedience, and material support. This file authors
 *   ONLY the tradition_scripture_reading as a clean, epsilon-invariant
 *   constraint; the sola_scriptura_reading and conciliar_reading are separate
 *   stories linked through network edges. The claim/metric gap is deliberate:
 *   claimed_type is asserted independently of the authored metrics, and the
 *   engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - catholic_magisterium: Agenda-setter and principal beneficiary (institutional/identity_locked) — defines doctrine, administers canon law, collects deference and material support
 *   - practicing_lay_faithful: Primary target (powerless/identity_locked) — bears interpretive exclusion, owes religious assent, funds the arrangement
 *   - diocesan_priesthood: Mediating beneficiary (organized/constrained) — administers reserved sacraments while sitting under hierarchical discipline
 *   - independent_catholic_theologians: Disciplined target (organized/constrained) — produces scholarship inside censorable bounds
 *   - culturally_catholic_laypeople: Attenuated participant (moderate/mobile) — episodic costs offset by rite-of-passage goods
 *   - womens_ordination_advocates: Excluded voice (organized/constrained) — central claim declared definitively settled beyond adjudication
 *   - abuse_survivor_advocacy_groups: Excluded voice (moderate/trapped) — demand accountability structures the office reserves to itself
 *   - academic_religious_scholars: Analytical observer (analytical/analytical) — maps the structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.6).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.28).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial Guardianship of the Deposit of Faith (Tradition-and-Scripture Reading)").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, '27a368a1-d21d-4e99-a662-7132adf0680b').
narrative_ontology:cs_kernel_codification('27a368a1-d21d-4e99-a662-7132adf0680b', fixed_text).
narrative_ontology:cs_authority_grounding('27a368a1-d21d-4e99-a662-7132adf0680b', lineage).
narrative_ontology:cs_interpretation_layer_present('27a368a1-d21d-4e99-a662-7132adf0680b').
narrative_ontology:cs_reading_relation('27a368a1-d21d-4e99-a662-7132adf0680b', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('27a368a1-d21d-4e99-a662-7132adf0680b', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('27a368a1-d21d-4e99-a662-7132adf0680b', foundational, tradition_indispensable_for_interpretation).
narrative_ontology:cs_axiom_status(tradition_indispensable_for_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('27a368a1-d21d-4e99-a662-7132adf0680b', tradition_indispensable_for_interpretation, theological).
narrative_ontology:cs_axiom('27a368a1-d21d-4e99-a662-7132adf0680b', secondary, religious_assent_owed_to_magisterium).
narrative_ontology:cs_axiom_status(religious_assent_owed_to_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('27a368a1-d21d-4e99-a662-7132adf0680b', religious_assent_owed_to_magisterium, conventional).
narrative_ontology:cs_reference_frame('27a368a1-d21d-4e99-a662-7132adf0680b', divinely_instituted_teaching_office).
narrative_ontology:cs_drift_state('27a368a1-d21d-4e99-a662-7132adf0680b', contemporary_post_conciliar_enforcement_decay, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27a368a1-d21d-4e99-a662-7132adf0680b', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, catholic_magisterium).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, diocesan_priesthood).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, practicing_lay_faithful).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, independent_catholic_theologians).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, culturally_catholic_laypeople).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, culturally_catholic_laypeople).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pope and bishops in communion with him define, guard, and adjudicate the deposit of faith: issuing doctrinal pronouncements, administering canon law, disciplining dissent from private admonition to excommunication, and receiving the deference, obedience, and financial support that flow to the teaching office. The office exists only inside this arrangement — a bishop who rejects magisterial authority ceases to hold it — and its members understand themselves as trustees of something received, not authors of a policy.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, catholic_magisterium, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, catholic_magisterium, beneficiary).

% Ordained men administer the sacraments the system reserves to clergy — Eucharist, absolution, anointing — drawing livelihood, local standing, and vocational identity from the mediation economy. They simultaneously sit under the hierarchy's discipline: assignments issued, celibacy required, loyalty expected, and a priest who publicly teaches against magisterial teaching loses faculty and income. Laicization is possible but carries social and economic cost.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, diocesan_priesthood, beneficiary,
    organized, biographical, constrained, regional).

% Devout laity receive all doctrine as handed down, owe religious assent of intellect and will to magisterial teaching, and access sacramental grace only through clergy. They fund parishes and dioceses through tithes and offerings. Their interpretive conclusions are licit only within magisterial bounds. Formed inside a sacramental biography — baptism, first communion, confirmation, marriage, funeral — exit means severing family, community, and self-understanding; most cannot conceive of it, and the collective lever their numbers represent (coordinated funding withdrawal) is rarely organized.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, practicing_lay_faithful, payer,
    powerless, biographical, identity_locked, global).

% Loosely attached members participate episodically — Christmas and Easter attendance, weddings, baptisms of children, funerals of parents — contributing donations when present and carrying residual obligations (family expectation, guilt, life-event logistics) without doctrinal engagement. They receive community and rite-of-passage goods while bearing little interpretive submission in practice; secular life is a fully available and socially uncosted alternative.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, culturally_catholic_laypeople, payer,
    moderate, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, culturally_catholic_laypeople, beneficiary).

% Academic and pastoral theologians trained inside Catholic institutions produce interpretive scholarship whose publication and teaching depend on imprimatur pathways or institutional toleration. The censure record — Leonardo Boff ordered to silence, Hans Kung removed from his Catholic theology faculty, Charles Curran's faculties revoked — prices conclusions outside magisterial bounds in careers. Their professional lives are built inside the system; exit to secular academies forfeits subject matter and community alike.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, independent_catholic_theologians, payer,
    organized, biographical, constrained, continental).

% Movements pressing for women's admission to ordained ministry operate inside Catholic space but outside the conversation that decides: the 1994 declaration Ordinatio Sacerdotalis pronounced the question definitively settled, placing their central claim beyond adjudication rather than answering it. They organize, publish, and protest at the boundary of a structure whose agenda-setting excludes the object of their claim.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, womens_ordination_advocates, excluded,
    organized, generational, constrained, global).

% Survivors of clerical abuse and their advocates demand accountability structures in which the hierarchy does not investigate and judge itself. They stand outside the governance conversation: canonical tribunals, internal review boards, and Rome-directed processes keep adjudication inside the office whose conduct is at issue. Trauma, dependence on the institution for acknowledgment, and public identification with the controversy bind many to the system they contest — the acknowledgment they seek is obtainable nowhere else.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, abuse_survivor_advocacy_groups, excluded,
    moderate, biographical, trapped, global).

% Historians and sociologists of religion trace the arrangement's development — from the regula fidei and episcopal succession through Trent's decree treating tradition as co-equal source, to ultramontane consolidation at Vatican I, to post-conciliar enforcement decay — mapping coordination and extraction functions without standing inside either.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, academic_religious_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, catholic_magisterium).
narrative_ontology:fixing_cost_class(biblical_authority__tradition_scripture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves doctrinal continuity and unity across a global, multilingual communion: supplies a single adjudicator for disputed scriptural readings, a stable canon of defined doctrine, standardized sacramental practice, and a transmission mechanism (seminaries, catechisms, catechesis) that forms each generation in the same faith.
% TRANSFER_FUNCTION: Moves interpretive authority and sacramental mediation from the laity to the ordained hierarchy — doctrine is received, not adjudicated, by the baptized, and grace is conveyed through clerically reserved sacraments — and moves material support (tithes, offerings, diocesan assessments, endowed property) and public deference upward to the teaching office.
% ABSENT_VOICES: Historically, condemned reformers were excluded by condemnation rather than answered — Jan Hus was burned at Constance under imperial-sanctioned safe conduct, and Luther's theses were met with excommunication rather than adjudicated debate. Today women's ordination advocates, abuse-survivor accountability movements, and censured theologians stand outside the deciding conversation; synodal consultations gather their input without transferring adjudication. Their absence lets institutional unanimity read as consent rather than as gatekeeping.
% DISAPPEARANCE_RATIONALE: If magisterial interpretive exclusivity vanished overnight, the world's largest Christian body would immediately confront the interpretive pluralization the arrangement exists to prevent: competing readings claiming equal warrant, sacramental-validity disputes, regionally divergent moral teachings, and a billion-plus-member communion reorganizing around new adjudication structures — the post-Reformation fragmentation pattern reproduced at global scale.
% FOUNDING_PROBLEM: From the earliest centuries: rival teachers — Gnostics, Marcionites, Donatists, Arians — read the same scriptures to opposite conclusions, each claiming apostolic warrant. The church needed a criterion distinguishing authentic apostolic teaching from counterfeit readings, and a mechanism for preserving that criterion across generations.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Protestant traditions, having rejected magisterial adjudication, promptly multiplied their own confessions, catechisms, and denominational courts — behavior attesting that the coordination problem is live; secular historians of Christianity document the same fragmentation dynamics; Orthodox and Catholic scholars agree on the problem's persistence while disputing the remedy. No serious source outside the dispute denies that rival scriptural interpretations continually arise.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.60 because the two extraction channels persist even after enforcement decayed: lay interpretive conclusions remain licit only inside magisterial bounds, and sacramental access runs exclusively through clergy. Suppression (raw, unscaled structural property — the engine scales only extractiveness by directionality and scope) ends at 0.28: the enforcement machinery that once included the Index of Forbidden Books, the Roman Inquisition, and censure careers is largely retired, replaced by rhetorical reaffirmation. Accessibility collapse is low (0.38) because alternatives were never globally closed after the Reformation: Protestant, Orthodox, and secular exits remain fully available, which distinguishes this construct sharply from a mountain. Resistance is high (0.68) — Reformation-scale rupture in the historical record, continuing dissent on both flanks (traditionalist rejection of conciliar reforms; progressive rejection of interpretive exclusion) in the present. Theater ratio (0.32) is rising as deference thins: an increasing share of magisterial activity maintains the appearance of command (documents, announcements, disciplinary language) relative to the deference actually commanded. The temporal series run on ONE shared grid (years since 1500 CE: 0=1500, 63=Trent closes, 100=peak Counter-Reformation enforcement, 270=Jesuit-suppression-era Enlightenment pressure, 370=Vatican I defines infallibility, 465=Vatican II, 525=present) so every tracked metric is authored at every examined point. The dynamics are phased-monotonic, not cyclical: enforcement rose to a Counter-Reformation peak and decayed thereafter; no intermittent-reinforcement mechanism is posited.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the magisterium's seat the arrangement is a trust it administers: coordination it did not invent but is charged to protect, with extraction experienced as stewardship burden. From the practicing-lay seat the same structure operates as enforced exclusion: every doctrinal conclusion is checked against an authority the layperson cannot petition except through channels the authority controls. The priesthood seat splits: beneficiary of the mediation economy (livelihood, standing, sacramental monopoly share) yet subject of the hierarchy's discipline (assignments, celibacy, loyalty expectations) — the derivation undersells this duality and the commentary records the limitation. Theologically-trained dissenters experience the constraint as career-defining censorship; culturally attached laity barely experience it at all. Coalition note: practicing laity are individually powerless but constitute the funding base — coordinated withholding (realized historically mainly where church taxes made payment legally compulsory) is the latent lever the power atom understates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (catholic_magisterium, diocesian_priesthood) drive low directionality for those seats; victim declarations (practicing_lay_faithful, independent_catholic_theologians, culturally_catholic_laypeople) drive high directionality, amplified by exit profiles: identity_locked exit pins practicing laity near the full-target end; constrained exit keeps theologians high; mobile exit moderates cultural laity. One directionality override is declared: the moderate-power atom is pinned to d=0.50 for the culturally Catholic seat, because the derivation from its victim-listing would overshoot toward full-target while its actual cost-bearing is episodic (occasional obligations, donation solicitations, family expectation) and offset by rite-of-passage goods — the seat sits near symmetric. The diocesan priesthood is left at its derived beneficiary-side d with a recorded limitation: the derivation cannot see intra-clerical discipline, so the seat's true d sits somewhat above the derived value. Vindicated propositions (apostolic succession, development of doctrine) are listed separately from beneficiaries: doctrines collect no rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rival readings of the same scriptures each claiming apostolic warrant — remains live, so this is not a mandatrophy case: the classification resists both mislabels. Calling the arrangement a pure snare erases the coordination function that even opponents demonstrate reliance on (Protestant traditions, having rejected magisterial adjudication, immediately rebuilt confessions, catechisms, and denominational courts to solve the same problem); calling it a pure rope erases the rent layer (interpretive exclusivity and sacramental mediation fees collected by the office that sets them). The post-conciliar trajectory bears watching: suppression falling (0.82 to 0.28) while theater rises (0.20 to 0.32) is the signature of possible piton-ward drift if the adjudication function hollows further — but the Catechism's issuance, ad limina visits, and active doctrinal congregations show the function still operating, and the R5 interview (founding_problem_status=live crossed with disappearance_verdict=world_rearranges) returns no mismatch flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the biblical_authority kernel — the tradition_scripture_reading. How would instantiating the sibling readings (sola_scriptura_reading, conciliar_reading) change the structural profile?',
    'Author the two sibling stories as separate constraints linked through the network edge set; compare computed per-seat classifications across the family.',
    'sola_scriptura_reading removes the clerical mediation layer (the hierarchy loses its beneficiary position; extraction cost relocates into doctrinal fragmentation borne diffusely by the whole communion). conciliar_reading retains the coordination function while moving adjudication from a permanent office to episodic councils, cutting concentrated capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the biblical_authority kernel; siblings are separate constraints, not folded into this one.').

omega_variable(
    interpretive_exclusivity_separability,
    'Is doctrinal unity structurally dependent on exclusive magisterial interpretation, or is unity separable from interpretive exclusivity?',
    'Comparative ecclesiology: Eastern Orthodoxy maintained substantial doctrinal continuity for a millennium without a single magisterium; Protestant polities display graded fragmentation rates. If durable unity persists under distributed adjudication, exclusivity is separable from coordination.',
    'If separable, the exclusivity layer is extraction riding a real coordination function, supporting tangled_rope over rope. If inseparable, part of the measured extraction is the price of the coordination itself and effective chi falls for every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_exclusivity_separability, empirical, 'Whether doctrinal unity requires interpretive monopoly.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the suppression constraining lay interpretive agency structural (canonical discipline, sacramental gating, censure) or internalized (formed docility — ''Rome has spoken'' reflexes that persist after barriers fall)?',
    'Post-exit trajectory studies of converts: if deference patterns persist after leaving magisterial jurisdiction, the suppression is substantially internalized; if interpretive agency arrives immediately upon exit, it was structural.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the constraint with them through exit, and the post-conciliar liberalization understates the constraint''s remaining grip.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural versus internalized suppression mechanism in lay formation.').

omega_variable(
    sacramental_mediation_necessity,
    'Does grace-conferral ontologically require clerical mediation (ex opere operato through ordained hands), or is the mediation requirement disciplinary policy administered by the hierarchy?',
    'Not resolvable empirically; it turns on tradition-internal theological commitment. Documented proxy: the system''s own practice variance — emergency baptism open to any person, lay-led services during priest shortages — reveals where mediation is treated as elastic.',
    'If ontological, sacramental extraction is intrinsic to the tradition''s own commitments and weighs as coordination cost; if disciplinary, the mediation requirement is removable policy and its rents are extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_mediation_necessity, conceptual, 'Whether clerical mediation of grace is constitutive or administrative.').

omega_variable(
    divine_institution_vs_construction,
    'Is magisterial interpretive authority a divinely instituted structure (presented within the tradition as unchangeable and Christ-founded) or a historically constructed arrangement whose present form emerged gradually?',
    'Historical development analysis: universal papal jurisdiction and infallibility were defined in 1870 against prior practice; the Council of Constance''s conciliar-superiority claim ran in the opposite direction. The form''s documented mutability can be compared against the immutability claim.',
    'If constructed, the constraint is false-summit shaped — presented as natural/divine while identifiable beneficiaries collect from it. If divinely instituted in the tradition''s own lights, the immutability claim is internally coherent and extraction assessment proceeds on different terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_institution_vs_construction, conceptual, 'Natural-law presentation versus historical construction of the teaching office.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 525).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t63, biblical_authority__tradition_scripture_reading, theater_ratio, 63, 0.12).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__tradition_scripture_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(bibl_tr_t270, biblical_authority__tradition_scripture_reading, theater_ratio, 270, 0.18).
narrative_ontology:measurement(bibl_tr_t370, biblical_authority__tradition_scripture_reading, theater_ratio, 370, 0.2).
narrative_ontology:measurement(bibl_tr_t465, biblical_authority__tradition_scripture_reading, theater_ratio, 465, 0.24).
narrative_ontology:measurement(bibl_tr_t525, biblical_authority__tradition_scripture_reading, theater_ratio, 525, 0.32).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(bibl_be_t63, biblical_authority__tradition_scripture_reading, base_extractiveness, 63, 0.64).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__tradition_scripture_reading, base_extractiveness, 100, 0.69).
narrative_ontology:measurement(bibl_be_t270, biblical_authority__tradition_scripture_reading, base_extractiveness, 270, 0.71).
narrative_ontology:measurement(bibl_be_t370, biblical_authority__tradition_scripture_reading, base_extractiveness, 370, 0.74).
narrative_ontology:measurement(bibl_be_t465, biblical_authority__tradition_scripture_reading, base_extractiveness, 465, 0.62).
narrative_ontology:measurement(bibl_be_t525, biblical_authority__tradition_scripture_reading, base_extractiveness, 525, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bibl_su_t63, biblical_authority__tradition_scripture_reading, suppression_requirement, 63, 0.7).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__tradition_scripture_reading, suppression_requirement, 100, 0.82).
narrative_ontology:measurement(bibl_su_t270, biblical_authority__tradition_scripture_reading, suppression_requirement, 270, 0.7).
narrative_ontology:measurement(bibl_su_t370, biblical_authority__tradition_scripture_reading, suppression_requirement, 370, 0.62).
narrative_ontology:measurement(bibl_su_t465, biblical_authority__tradition_scripture_reading, suppression_requirement, 465, 0.35).
narrative_ontology:measurement(bibl_su_t525, biblical_authority__tradition_scripture_reading, suppression_requirement, 525, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the kernel biblical_authority decomposes into three readings with distinct epsilon and distinct victim sets. This file authors only tradition_scripture_reading (high clerical extraction; centralized adjudication; lay interpretive agency as victim). sola_scriptura_reading strips the hierarchical extraction layer but relocates cost into doctrinal fragmentation borne diffusely by the whole communion; conciliar_reading retains the coordination diagnosis while moving adjudication into episodic councils, reducing concentrated capture. Edge direction: the Reformation-era repudiation of THIS reading produced the sola_scriptura sibling (upstream to downstream), while the conciliar sibling shares this reading's coordination premise and diverges only on the locus of authority. All three files link mutually through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
