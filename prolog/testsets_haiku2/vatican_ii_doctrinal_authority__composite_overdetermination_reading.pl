% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Composite Doctrinal Authority Structure (Overdetermination Reading)
 *   domain: ecclesiology/institutional-history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962–1965) is presented by the Roman Curia and ecclesiastical
 *   traditionalists as a unified reform, and by progressives as authorization
 *   for ongoing doctrinal evolution. This reading rejects that binary
 *   framing. The Council was not one constraint but structural
 *   overdetermination: it simultaneously addressed four distinct doctrinal
 *   questions—the Church's relationship to worship (Sacrosanctum Concilium),
 *   to other Christian communions (Unitatis Redintegratio), to its own
 *   authority (Lumen Gentium on collegiality), and to modernity and religious
 *   freedom (Gaudium et Spes, Dignitatis Humanae). Each component has
 *   independent extractiveness, independent beneficiary/victim structure, and
 *   independent degree of rupture or continuity from pre-conciliar doctrine.
 *   The 'continuity vs. rupture' debate treats the Council as a single object
 *   with a single property; this reading argues that treating it so is a
 *   category error born of bureaucratic packaging. The constraint's operation
 *   (managing interpretation of the documents, enforcing boundaries between
 *   authorized and heterodox readings) extracts from multiple parties by
 *   leaving each component's status undecided and making interpretive
 *   authority itself contestable.
 *
 * KEY AGENTS:
 *   - Roman Curia: institutional beneficiary, sets interpretive boundaries, maintains authority over what counts as 'authentic' development
 *   - Episcopal collegiality proponents: benefit from textual grants of authority, pay in enforcement friction with Curial primacy
 *   - Traditionalist communities: bear marginalization costs, identity-locked exit prevents real alternative
 *   - Progressive reform movements: pay in doctrinal investigation and career restriction, have more exit options than traditionalists
 *   - Vatican II text: ambiguous document, benefits all parties by offering textual warrant to each reading
 *   - Laity and lower clergy: powerless, pay in enforcement confusion where interpretation diverges from doctrine
 *   - Ecumenical bodies: excluded from authoritative reading, their interests affected but voice constrained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.42).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Composite Doctrinal Authority Structure (Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiology/institutional-history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '690e9e0b-6604-4b81-9a46-58e4b3c33266').
narrative_ontology:cs_kernel_codification('690e9e0b-6604-4b81-9a46-58e4b3c33266', fixed_text).
narrative_ontology:cs_authority_grounding('690e9e0b-6604-4b81-9a46-58e4b3c33266', lineage).
narrative_ontology:cs_interpretation_layer_present('690e9e0b-6604-4b81-9a46-58e4b3c33266').
narrative_ontology:cs_reading_relation('690e9e0b-6604-4b81-9a46-58e4b3c33266', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('690e9e0b-6604-4b81-9a46-58e4b3c33266', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('690e9e0b-6604-4b81-9a46-58e4b3c33266', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_axiom('690e9e0b-6604-4b81-9a46-58e4b3c33266', foundational, vatican_ii_multiple_distinct_doctrinal_shifts).
narrative_ontology:cs_axiom_status(vatican_ii_multiple_distinct_doctrinal_shifts, holdable).
narrative_ontology:cs_axiom_grounding('690e9e0b-6604-4b81-9a46-58e4b3c33266', vatican_ii_multiple_distinct_doctrinal_shifts, empirically_contingent).
narrative_ontology:cs_axiom('690e9e0b-6604-4b81-9a46-58e4b3c33266', secondary, component_extractiveness_independence).
narrative_ontology:cs_axiom_status(component_extractiveness_independence, holdable).
narrative_ontology:cs_axiom_grounding('690e9e0b-6604-4b81-9a46-58e4b3c33266', component_extractiveness_independence, empirically_contingent).
narrative_ontology:cs_reference_frame('690e9e0b-6604-4b81-9a46-58e4b3c33266', pre_conciliar_ecclesial_clarity).
narrative_ontology:cs_drift_state('690e9e0b-6604-4b81-9a46-58e4b3c33266', contemporary_post_vatican_ii_60years, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('690e9e0b-6604-4b81-9a46-58e4b3c33266', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia_institutional_stability).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, episcopal_collegiality_proponents).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, laity_and_lower_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, episcopal_collegiality_proponents).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, laity_and_lower_clergy).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, doctrinal_development_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, episcopal_authority_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The papal bureaucracy that convened, shaped, and ratified Vatican II's documents. It maintains interpretive authority over the Council's meaning and enforces the boundary between authorized and heterodox readings. It balances central authority with the Council's grant of collegial powers to bishops, and controls what counts as 'authentic' development versus rupture or heterodoxy.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia, agenda_setter,
    institutional, civilizational, trapped, universal).

% Bishops and theologians who read Vatican II as granting real shared governance and doctrinal authority to the episcopate. They benefit from the Council's texts affirming episcopal collegiality, but pay in enforcement friction when their interpretation conflicts with Curial primacy or conservative papal doctrine. Their exit is constrained by ordination vows and institutional identity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, episcopal_collegiality_proponents, beneficiary,
    organized, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, episcopal_collegiality_proponents, payer).

% Communities (SSPX, sede vacantist groups, Latin Mass devotees) who read Vatican II as a rupture from pre-conciliar doctrine and discipline. They bear the cost of marginalization, restricted sacramental access, and doctrinal condemnation. Their exit is identity-locked: leaving the Church entirely or adopting the Council's framework would mean abandoning their constitutive self-concept as defenders of true doctrine.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_communities, payer,
    moderate, generational, identity_locked, regional).

% Theologians, clergy, and movements invoking the 'spirit of the Council' to justify reforms beyond or contrary to the Council's texts (married clergy, women's ordination, contraceptive access, LGBTQ+ inclusion). They bear costs in doctrinal investigation, career restriction, and exclusion from teaching roles when their reading exceeds Curial tolerance. They have more exit options than traditionalists: some leave for secular academia, some transition to Protestant denominations.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_reform_movements, payer,
    moderate, biographical, mobile, national).

% The 16 conciliar documents function as a legitimacy source that all sides cite. The documents' notorious ambiguities—on collegiality, on papal primacy, on the relationship between continuity and change—mean the text itself resists univocal reading. The text benefits all parties by granting each textual warrant for their reading, while constraining all to claim fidelity to it.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_text_itself, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_text_itself).

% Non-Catholic Christian churches and Orthodox churches that are affected by the Council's ecumenical openings (Unitatis Redintegratio, Nostra Aetate) but have no voice in interpreting Vatican II or constraining what counts as authentic development of its ecumenical vision. Their own readings of the Council's meaning are not binding on Catholic interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_bodies, excluded,
    organized, generational, constrained, universal).

% Parish-level Catholics, religious women, and lower clergy live within the enforcement space where diocesan and Vatican policies interpret and implement the Council. They benefit from liturgical accessibility and ecumenical openness in the documents, but pay in enforcement confusion where local interpretation diverges from Curial doctrine. Leaving requires abandoning religious identity or community.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, laity_and_lower_clergy, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, laity_and_lower_clergy, beneficiary).

% Academic scholars who analyze Vatican II's composition, theological antecedents, and historical impact. They observe and report on the structural overdetermination without adjudicating which reading is correct. Their work enables other seats to contest authoritative interpretations.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theological_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempted unified coordination of the global Catholic Church's relationship to modernity, ecumenism, and its own authority structure after centuries of defensive posture. Vatican II was convened to address fragmentation between progressive theologians and institutional conservatism, and between Catholic teaching and modern democratic pluralism.
% TRANSFER_FUNCTION: Redistributes interpretive authority: from papal Curia alone toward the episcopate (Lumen Gentium on collegiality); from magisterial rigidity toward doctrinal development (Dei Verbum); from exclusivist ecclesiology toward ecumenical recognition (Unitatis Redintegratio); and from liturgical uniformity toward local adaptation (Sacrosanctum Concilium). Each transfer empowers some seats and constrains others.
% ABSENT_VOICES: Vatican II was decided by all-male hierarchical process excluding women religious, laity, and non-Catholic voices whose interests the documents touch. The progressive wing includes women theologians marginalized from drafting; the traditionalist wing includes lay movements frozen out of reform debates. Ecumenical churches were consulted but held no voting power. Their absence from the drafting process structures what counts as 'authentic' interpretation later.
% DISAPPEARANCE_RATIONALE: Vatican II's disappearance would constitute a world-rearrangement because the authority structure it established (episcopal collegiality, doctrinal development framework, ecumenical legitimacy) is now institutional fact. But whether the Council's actual impact or its formal documents matter more is what the reading contest is about: traditionalists argue the 'spirit' disappeared into heterodoxy and only doctrinal restoration matters; progressives argue the documents are the scaffold for ongoing reform; conservatives argue the documents' intention can be recovered by strict textual reading.
% FOUNDING_PROBLEM: Vatican I (1870) had left unresolved the relationship between papal infallibility and episcopal authority, between doctrinal development and unchanging deposit of faith, between the Church's institutional integrity and its dialogue with modern thought. Vatican II was convened by John XXIII to address these tensions and to modernize the Church's relationship to pluralism, ecumenism, and democratic governance.
% FOUNDING_PROBLEM_CORROBORATION: Vatican II's historical records confirm the tensions it addressed. The progressive bishops and theologians cite the documents as evidence the founding problems (Church's rigidity, ecumenical isolation, clerical monopoly on interpretation) were real and the Council addressed them. The Curia attests the problems were reconciliation of tradition with legitimate development. Traditionalists argue the founding problem was posed wrongly—the real problem was loss of faith, not lack of openness. Historians outside the institutional beneficiary set (academic theologians, ecumenical observers' testimony) confirm the Council's texts were composed under internal tension and bear marks of compromise between incompatible positions.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measure (0.58 at interval end) reflects the constraint's core mechanism: the ambiguous text and the Vatican's gatekeeping over interpretation combine to extract from every party by preventing any stable reading from consolidating. The progressives cannot claim the 'spirit of the Council' without risking doctrinal investigation; traditionalists cannot reject the Council without breaking communion. The suppression requirement (0.42 at interval end) is moderate because the constraint relies on doctrinal authority and bureaucratic enforcement rather than external coercion—the Church has no police. Theater ratio (0.48) rises through the first 30 years as interpretive debates become increasingly performative (conservative popes invoking 'hermeneutics of continuity,' progressive theologians claiming the Council was 'never implemented'), peaks around year 30 (pontificate shifts, reform momentum stalls), then stabilizes around 0.48 as the theater becomes the steady state—endless reinterpretation of the Council's meaning with little material change. Accessibility collapse (0.61) is moderate: the Council's texts are public and anyone can read them, but interpretive authority is collapsed (you need the Church's permission to claim fidelity to the Council). Resistance (0.72) is high because the constraint's ambiguities activate genuine doctrinal disagreement—people genuinely believe their reading is correct, not that they are being extracted from. The measurement series spans 60 years (approximate duration from Vatican II through the papacy of Francis, where the constraint's operation is most visible) and tracks how extractiveness rises in the decades immediately after the Council, then stabilizes at a high but non-increasing level as the constraint becomes institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   From the Curia's seat, the constraint protects authentic development within the Catholic tradition—a real coordination function. From the traditionalist seat, the constraint is a cover for heterodoxy. From the progressive seat, the constraint is suppression of legitimate doctrinal growth. From the laity's seat, the constraint is simply confusion—which authority do I follow when bishops and Rome contradict each other? The engine's per-seat classification will reflect these structural positions differently, computing the constraint as rope (minimal extraction, genuine coordination) from the Curia's seat and as snare (pure extraction under cover of coordination) from the traditionalist and progressive seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The Roman Curia benefits from the constraint's ambiguities because ambiguity preserves Curial interpretive authority: if the Council clearly authorized collegiality, the Curia's primacy would be diminished; if the Council clearly rejected collegiality, the progressives would lose textual warrant. The ambiguity keeps both sides claiming fidelity to the Council and therefore keeps interpretive disputes within the Curia's purview. Episcopal collegiality proponents and progressive reformers are constrained by the same ambiguities: they can cite the text, but the Curia can deny their reading authenticity. Traditionalist and progressive communities both pay, but in opposite ways: traditionalists pay for rejecting the Council's authority, progressives pay for exceeding it. The constraint's enforcement—doctrinal investigations, career restrictions, exclusion from teaching—falls unequally on the most vulnerable seats (lower clergy, laity, reformers without institutional backing). Beneficiaries maintain the constraint through interpretive gatekeeping; victims would need either unified doctrinal consensus (impossible given the overdetermination) or external pressure (not available within ecclesiastical structures) to change it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling tradition with modernity, papal authority with collegial governance, unchanging doctrine with legitimate development) remains live, but the constraint's response has degraded. The constraint cannot solve the problem—its ambiguities reproduce the problem rather than resolving it. Decades of hermeneutical labor (John Paul II's 'hermeneutics of continuity,' Francis's 'accompaniment' rhetoric) have not stabilized interpretation. The theater ratio rises precisely because the constraint's function has atrophied: it now serves primarily to manage disputes rather than to settle them. The measurement series shows extractiveness rising for three decades, then stabilizing, which tracks the constraint becoming institutionalized—the disputes don't resolve, but the institutional machinery for managing them becomes routine. The constraint persists because every party finds something to claim fidelity to in the ambiguous text, and because leaving the Church entirely (the only real exit for traditionalists and some progressives) costs more than bearing the enforcement friction. This is the signature of a tangled_rope approaching piton-hood: the constraint's extractive function persists, but the coordination benefit has become vestigial—the ongoing reinterpretation is now mostly theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_vs_incoherence,
    'Is the Council''s structural contradiction (apparent incompatibility between collegiality language and papal primacy dogma; between continuity-of-substance language and liturgical rupture) a feature of overdetermination—multiple distinct commitments packaged together—or a bug in the Council''s composition (true incoherence born of compromise)?',
    'Historical analysis of the drafting process and the theological antecedents of each document. If the documents were drafted separately by different theological commissions with different axioms, overdetermination is the right frame. If one unified theological vision broke down under pressure to reach consensus, incoherence is the right frame.',
    'If overdetermination, the constraint''s extractiveness derives from the packaging of distinct commitments into one authority structure, and different parties'' incompatible readings are structural, not errors. If incoherence, the constraint''s extractiveness derives from doctrinal confusion and the Curia''s use of ambiguity to suppress clarity—the constraint becomes a snare with a thinner coordination story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_vs_incoherence, empirical, 'Whether contradictions in Vatican II are structural packaging or compositional failure.').

omega_variable(
    component_independence_measurement,
    'Can the four component constraints (liturgical authority structure, ecumenical opening, collegiality, modernization) be measured independently with distinct ε values, or does measuring one component change the measurement of the others?',
    'Generate four separate constraint stories, one per component, and measure each independently. If the ε values are stable across the four stories and do not sum to the overall Vatican II ε, then the components are genuinely independent constraints overdetermined into one institutional structure. If measuring components separately induces feedback (e.g., measuring the collegiality constraint changes how the liturgy constraint is enforced), then the components are coupled and overdetermination is a misleading frame.',
    'If independent, this reading''s proposal to decompose Vatican II is valid and productive—the constraint family would comprise four stories. If coupled, the decomposition obscures structural coupling and the single-constraint frame (with amplified ambiguity) is more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(component_independence_measurement, conceptual, 'Whether Vatican II''s components admit independent constraint measurement.').

omega_variable(
    identity_lock_mechanism_traditionalist,
    'Do traditionalist communities'' identity-locked exit (identity_locked in stakeholder.exit_options) derive from doctrinal conviction or from social/relational identity fusion with the pre-conciliar Church? If the identity lock were broken—if they could see pre-conciliar and post-conciliar Catholicism as continuous—would their exit options expand?',
    'Post-exit trajectory analysis: when traditionalists leave the Church (for SSPX, sede vacantism, or Protestantism), do they retain the doctrinal convictions in new institutional homes, or does the identity component dissolve? If they retain doctrinally coherent positions, identity lock was doctrinal. If they fragment or depoliticize, identity lock was relational/community-based.',
    'If identity lock is doctrinal, their marginalization reflects genuine disagreement about the Council''s meaning and is not suppression per se. If identity lock is relational, their suppression costs are higher than metrics capture—the constraint extracts from their identity, not just their doctrinal freedom. This informs whether the constraint is tangled_rope (genuine disagreement in coordination) or snare (suppression of identity-constituted opposition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_traditionalist, empirical, 'The source and depth of traditionalist communities'' identity lock in the constraint.').

omega_variable(
    reading_contest_legitimacy,
    'Does the Church''s framework (as embodied in the Curia and magisterium) legitimately authorize all four sibling readings as live interpretations, or does it de facto privilege one reading (continuity_reading or hermeneutics_of_continuity) while packaging the others as heterodox errors?',
    'Audit official Curial statements, papal encyclicals, and doctrinal office decisions over 60 years to identify which reading receives doctrinal preference. If the Curia explicitly affirms that multiple readings are legitimate, all four readings coexist. If the Curia explicitly condemns one or more readings as erroneous, those readings'' legitimacy is not independent but constrained by institutional power.',
    'If all four readings are de jure legitimate, the constraint is genuinely tangled_rope with ambiguous authority structure. If the Curia de facto privileges one reading, the constraint is more extractive than measured—it uses doctrinal authority to suppress readings the text could support. This would raise ε and change the claimed_type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_legitimacy, empirical, 'Whether Vatican II''s institutional authority genuinely permits multiple readings or de facto suppresses some.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t45, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 45, 0.52).
narrative_ontology:measurement_basis(vati_tr_t45, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t45, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 45, 0.59).
narrative_ontology:measurement_basis(vati_be_t45, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t45, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 45, 0.43).
narrative_ontology:measurement_basis(vati_su_t45, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.18).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% DUAL FORMULATION NOTE:
% Vatican II doctrinal authority is a contested kernel with four distinct constraint readings: this story (composite_overdetermination) models Vatican II as multiple overdetermined structural shifts packaged as unified reform. The sibling readings (continuity, progressive rupture, traditionalist rupture) each model Vatican II as a single unified shift with a single extractiveness value. The four stories are linked by network.affects_constraints; each reads the same historical event but decomposes it structurally differently. Decomposition follows ε-invariance principle: if measuring Vatican II via distinct components yields distinct ε values, then treating it as one constraint is ε-noninvariant—four constraints, not one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
