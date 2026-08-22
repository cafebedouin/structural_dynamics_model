% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Reformation Theological Fragmentation: Denominational Incompatibility and Doctrinal Authority
 *   domain: religious/historical/epistemological
 *
 * SUMMARY:
 *   This reading treats the Reformation as fundamentally a theological event:
 *   the constraint emerges from the fact that competing soteriological
 *   (salvation doctrine) and ecclesiological (church structure) positions
 *   became mutually incompatible, generating structurally irreconcilable
 *   denominations. Luther's forensic justification, Calvin's predestination,
 *   the Catholic response at Trent, the Radical Reformation's ecclesiology,
 *   and Zwingli's memorialist Eucharist are not rhetorical differences or
 *   political covers — they are logically exclusive answers to the same
 *   theological questions. The constraint's operation is the process by which
 *   these doctrinal incompatibilities crystallize into institutional
 *   denominational boundaries, confessional documents that codify and enforce
 *   those boundaries, and the suppression machinery (legal, political,
 *   intellectual) that maintains them. The beneficiaries are the reformed
 *   denominational leaderships, whose institutional authority is constituted
 *   through doctrinal distinction. The victims are those subject to the
 *   newly-hardened sectarian boundaries: lay believers whose salvific status
 *   and communal identity are now contested, and the aspiration toward
 *   universal Christian doctrine. This reading is deliberately ONE account of
 *   the Reformation — not the political realignment reading (which centers
 *   sovereignty and nation-state emergence) or the technological reading
 *   (which centers printing and information diffusion). Each reading of this
 *   kernel is a separate constraint, with its own ε, its own
 *   beneficiary/victim structure, and its own interpretation of what made the
 *   Reformation happen.
 *
 * KEY AGENTS:
 *   - reformed_denominational_leadership: agenda-setters who codify and enforce doctrinal boundaries; beneficiary of institutional authority through sectarian distinctiveness
 *   - roman_curia_and_episcopal_establishment: institutional payer bearing suppression costs to maintain threatened doctrinal monopoly
 *   - lay_communicants: powerless, identity-locked victims of doctrinal incompatibility; salvation understanding becomes contested and sectarian
 *   - princes_and_political_authorities: dual-positioned as both agenda-setters (enforcing state religion) and beneficiaries (gaining sovereignty against papal authority)
 *   - theological_scholars_and_polemicists: beneficiaries of professional status through articulating and defending doctrinal positions
 *   - printing_industry: beneficiary of increased demand for confessional and polemical texts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.68).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.52).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation Theological Fragmentation: Denominational Incompatibility and Doctrinal Authority").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "religious/historical/epistemological").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '1a330a94-9132-4373-ab5f-ecb1a1a85e19').
narrative_ontology:cs_kernel_codification('1a330a94-9132-4373-ab5f-ecb1a1a85e19', fixed_text).
narrative_ontology:cs_authority_grounding('1a330a94-9132-4373-ab5f-ecb1a1a85e19', lineage).
narrative_ontology:cs_interpretation_layer_present('1a330a94-9132-4373-ab5f-ecb1a1a85e19').
narrative_ontology:cs_reading_relation('1a330a94-9132-4373-ab5f-ecb1a1a85e19', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_reading_relation('1a330a94-9132-4373-ab5f-ecb1a1a85e19', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('1a330a94-9132-4373-ab5f-ecb1a1a85e19', foundational, doctrinal_truth_determines_ecclesial_bounds).
narrative_ontology:cs_axiom_status(doctrinal_truth_determines_ecclesial_bounds, holdable).
narrative_ontology:cs_axiom_grounding('1a330a94-9132-4373-ab5f-ecb1a1a85e19', doctrinal_truth_determines_ecclesial_bounds, deontological).
narrative_ontology:cs_axiom('1a330a94-9132-4373-ab5f-ecb1a1a85e19', secondary, scripture_sufficient_for_doctrine).
narrative_ontology:cs_axiom_status(scripture_sufficient_for_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('1a330a94-9132-4373-ab5f-ecb1a1a85e19', scripture_sufficient_for_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('1a330a94-9132-4373-ab5f-ecb1a1a85e19', unified_christendom_under_roman_authority).
narrative_ontology:cs_drift_state('1a330a94-9132-4373-ab5f-ecb1a1a85e19', post_westphalian_confessional_equilibrium_1648, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('1a330a94-9132-4373-ab5f-ecb1a1a85e19', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, reformed_denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, emergent_confessional_traditions).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, universal_church_unified_doctrine_aspiration).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, lay_communicants_navigating_sectarian_division).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, princes_and_political_authorities).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, theological_scholars_and_polemicists).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, printing_industry_and_publishers).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, roman_curia_and_episcopal_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutes the reformed ecclesial bodies (Lutheran, Reformed, Radical Reformation churches) that crystallize around distinct soteriological and ecclesiological doctrines. They set the boundaries of acceptable belief, codify doctrine in confessional statements, and enforce doctrinal purity through excommunication, exile, or legal sanction. Their institutional existence depends on maintaining doctrinal distinction from Rome and from rival protestant traditions.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, reformed_denominational_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).

% Bears the cost of defending universal doctrine against irreconcilable competitors within Christendom. Previously held monopoly on authorized interpretation; now forced to define and defend specific positions (Trent, canon law) against multiple internally-coherent alternative doctrinal frameworks. Must actively suppress or politically contain reformed denominations to maintain territorial and spiritual authority.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, roman_curia_and_episcopal_establishment, payer,
    institutional, generational, constrained, continental).

% Ordinary believers whose salvation doctrine, church attendance, and communal identity are now contested and subject to irreconcilable theological positions. A convert from Catholicism to Lutheranism now inhabits a confessional boundary with no neutral ground; their salvation understanding, ritual participation, and social standing are all re-categorized by doctrinal shift. They cannot be simultaneously Catholic and Lutheran; the frameworks are incompatible at the soteriological core.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, lay_communicants_navigating_sectarian_division, payer,
    powerless, biographical, identity_locked, local).

% Adopt reformed denominations as state religion and enforce doctrinal boundaries through law and excommunication. They benefit from the theological fragmentation insofar as it enables sovereignty claims against the papacy. They also bear enforcement costs — establishing a territorial religion requires defining and defending its doctrinal borders and suppressing rival denominations within their realm.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, princes_and_political_authorities, agenda_setter,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, princes_and_political_authorities, beneficiary).

% Gain professional status, patronage, and intellectual purpose from articulating and defending distinct doctrinal positions. They produce the confessional documents that codify denominational boundaries. Their careers depend on the existence of irreconcilable theological problems: once a doctrinal question is settled within their tradition, their energies turn to defending it against external critics and internal heterodoxy.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, theological_scholars_and_polemicists, beneficiary,
    organized, biographical, constrained, continental).

% Profit from the multiplication of confessional and polemical texts, scriptural commentaries, and catechisms. Denominational proliferation drives demand for more books. They remain neutral on theology itself but are economically invested in doctrinal controversy sustaining publication.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, printing_industry_and_publishers, beneficiary,
    powerful, biographical, mobile, continental).

% The ideal of a reunified Christendom under a single doctrinal framework. This aspiration is analytically present but institutionally excluded from the constraint story: no actor in the reformation period commands the authority to enforce ecumenical reunion across the fragmented denominations; the gap between the aspiration and the achievable is the structural opening the theological fragmentation fills.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenical_reconciliation_aspiration, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(reformation_composite__theological_fragmentation_reading, ecumenical_reconciliation_aspiration).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, reformed_denominational_leadership).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable framework for resolving soteriological disputes: rather than allow infinite doctrinal variation within a single communion, the constraint crystallizes irreconcilable positions into distinct ecclesiastical bodies, each with coherent internal doctrine, clear membership boundaries, and recognized leadership. This solves the coordination problem of 'who decides doctrine?' by fragmenting the question — each denomination answers internally; Rome, Wittenberg, Zurich, Geneva, and the Radical communities each establish clear chains of interpretation authority.
% TRANSFER_FUNCTION: Transfers interpretive authority from a centralized institutional monopoly (Rome) to a pluralistic set of denominational authorities, each claiming apostolic continuity and scriptural fidelity. Transfers doctrinal labor from Rome's magisterium to a distributed network of reformed leadership and theological scholars. Transfers institutional loyalty from Christendom-wide communion to sectarian belonging. Transfers spiritual certification (who is saved, what constitutes righteous living) from sacramental mediation to doctrine-specific faith understanding.
% ABSENT_VOICES: Lay believers who do not read scripture, do not engage in theological reasoning, and who are indifferent to soteriology — they are shut out of the theological debate itself. Anabaptist and radical reformation voices are present in some regions but systematically suppressed or marginalized by both Catholic and magisterial Protestant authorities. Jewish and Muslim intellectual traditions, which engage the same texts and questions, are explicitly excluded from Christian theological dispute.
% DISAPPEARANCE_RATIONALE: If the theological fragmentation constraint vanished — if the three soteriological and ecclesiological readings were all declared equally valid within a single communion — the reformation as historically understood could not have occurred. The political realignments, territorial churches, and confessional wars would not have happened in their actual form. Removing the theological incompatibility does not restore pre-reformation Christendom (printing and political consolidation would still have fractured authority), but it reframes the entire historical trajectory: Europe reorganizes around different denominational boundaries or remains under negotiated Catholic-reformed federation rather than hardened sectarian division.
% FOUNDING_PROBLEM: Which doctrinal framework correctly understands salvation? Specifically: (1) Is righteousness forensic (imputed justification by faith) or infused (gradual sanctification)? (2) Who mediates between God and humanity — a priesthood, the believer's conscience, or scriptural text? (3) What is the ontological status of the Eucharist? (4) What is the locus of doctrinal authority — papal magisterium, ecumenical councils, scripture alone, the spirit-led individual conscience?
% FOUNDING_PROBLEM_CORROBORATION: Reformed denominational theologians (Luther, Calvin, Zwingli, their successors) attest these questions remain live and unsolved by Catholic answers. Catholic councils (especially Trent) attest the same questions and assert their own doctrinal solutions. From OUTSIDE the denominational beneficiary seats: historians of doctrine (Charles Hodge, Jaroslav Pelikan, David Bagchi) corroborate that the reformation period genuinely turned on irreconcilable answers to these questions, not merely on political leverage or printing technology. The founding problem persists at least to the present (ecumenical dialogue continues; full communion remains unachieved across traditions).
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (pre-1520: theological dissent exists but is minority and containable) to 0.68 (post-1580: fragmentation is institutional, hardened, and global). The slope accelerates 1520–1580 as the doctrinal disputes are converted into confessional documents (Augsburg Confession 1530, Reformed confessions, Council of Trent 1545–1563) that formalize incompatibility and establish denominational machinery to enforce it. The plateau at 0.68 reflects the Westphalian settlement (1648): theological incompatibility is still high, but its extractive force is now managed through territorial-church arrangements and political equilibrium rather than through escalating suppression. Suppression requirement is lower than extractiveness because much of the constraint's operation is self-maintaining through ideology and identity-fusion (lay communicants are taught their denomination's doctrine is salvifically necessary), not pure coercion. Theater ratio rises from near-zero (early theological disputation is genuine intellectual labor) to 0.41 (by 1648, confessional documents are partly ritual performance — the real work is political compromise, not theological reconciliation). The accessibility collapse metric (0.71) reflects the historical fact that once doctrinal positions crystallize and denominational boundaries harden, alternatives become increasingly difficult to perceive or exit — a person born into Lutheran or Catholic community faces identity-locked alternatives. Resistance (0.58) is substantial: anabaptists resist both magisterial churches; some reformers (radical reformation figures) resist the institutional crystallization itself; the Catholic counter-reformation fights back; lay resistance takes the form of folk syncretism and denominational indifference in practice despite official doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The temporal dimension reveals a perspectival shift: in 1520, the constraint is experienced by Rome as heretical dissent that can be contained. By 1580, it is experienced as institutional fragmentation that cannot be reversed. By 1648, it is experienced as a stable confessional pluralism that must be managed diplomatically rather than suppressed militarily. This is not three different constraints — it is the same theological incompatibility constraint operating at different scales of institutionalization. The reading stays constant; the operational experience changes.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformed denominational leadership: d approaches 0.0 (full beneficiary). They author the constraint's doctrinal core and benefit from the institutional authority it creates. Exit for them means theological surrender, which is identity-locked because their professional and spiritual identity is constituted through doctrinal distinctiveness. Roman establishment: d approaches 1.0 (full target). They lose the monopoly on interpretation, pay suppression costs, and must defend positions they previously took for granted. Exit for them means accepting theological diversity, which they experience as doctrinal defeat. Lay communicants: d = 0.6–0.7 (target end). They are trapped by identity-fusion (their salvation understanding, communal belonging, and family relationships are all denominations-specific) and bear the cost of sectarian division without authoring it. Princes: d = 0.3–0.4 (moderate-beneficiary). They leverage theological fragmentation for sovereignty but also bear suppression costs. They have mobile exit (they can shift alliances or religions) so directionality is modulated downward from what pure payer status would suggest. Theological scholars: d = 0.2–0.3 (beneficiary end). They benefit from the intellectual labor the constraint creates and have constrained but mobile exit (they can move between courts, find patronage in different denominations, or shift their intellectual focus). Printing industry: d = 0.1–0.2 (beneficiary end). They profit and have mobile exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the false-summit error: it does not claim the theological fragmentation is a natural law or inevitable feature of Christianity. It explicitly treats it as a constraint with identifiable beneficiaries (denominational leadership, scholars, printers) and victims (lay believers, the ecumenical aspiration, Rome's institutional monopoly). The constraint's founding problem (which soteriological framework is correct) is treated as LIVE, not as a question whose answer should be obvious or natural. This prevents mandatrophy: the constraint is still doing the work it was built to do (resolving doctrinal disputes by separating incompatible communities), even though its costs have become salient (sectarian division, suppression machinery, theological fragmentation as inherited institutional form). If the founding problem had become dead (if ecumenical theology had decisively settled all doctrinal disputes), the constraint would transition to piton — still operating theatrically, but no longer serving its founding function. The reading does not (yet) declare mandatrophy resolved because the reformation's theological disputes remain live in the present (different denominations still hold irreconcilable soteriologies).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_vs_identity_fusion,
    'To what extent is lay doctrinal conformity enforced through intellectual conviction vs. identity-fusion (belonging, inheritance, social pressure)? Is suppression structural (legal consequences of heterodoxy) or internalized (the believer has been taught to want denominational orthodoxy)?',
    'Post-Enlightenment secularization and denominational switching patterns: if believers abandon their inherited denomination when legal suppression is removed, suppression was structural; if they maintain confessional identity despite decriminalization, the suppression is substantially internalized through identity.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than the structural metric suggests — lay communicants carry the constraint with them even after legal enforcement weakens. This affects whether the constraint is best classified as Tangled Rope (coordinated + extracted) or Snare (pure extraction with identity cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_identity_fusion, empirical, 'Structural vs. internalized mechanism of doctrinal enforcement on lay believers').

omega_variable(
    theological_incompatibility_necessity,
    'Are the soteriological and ecclesiological positions of different reformations genuinely logically incompatible, or are they differences in emphasis and metaphor that could be reconciled through reinterpretation?',
    'Formalization of the doctrinal statements and application of modern logic: can both Catholics and Calvinists affirm the same propositions if their doctrines are translated into formal logic? Or do they necessarily assert contradictory propositions about justification, mediation, and authority?',
    'If genuinely incompatible, the theological constraint is primary and explains the institutional fragmentation. If reconcilable through reinterpretation, the fragmentation is not forced by theology but by institutional commitment to maintaining boundaries — the constraint would be reclassified as institutional extraction using theology as cover (closer to Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_incompatibility_necessity, conceptual, 'Whether the reformation''s doctrinal positions are logically incompatible or reconcilable through reinterpretation').

omega_variable(
    beneficiary_vs_side_effect,
    'Do denominational leaders deliberately crystallize doctrinal boundaries to extract authority and prestige, or do the boundaries crystallize as a side effect of defending theological positions they genuinely believe are salvifically necessary?',
    'Historical analysis of private correspondence, decision-making records, and comparative institutional behavior: did reformers prioritize theological accuracy or institutional distinctiveness when facing pressure to reconcile? Did Catholic bishops at Trent prioritize doctrinal truth or institutional authority recovery?',
    'If deliberate extraction, the beneficiary claim is straightforward and the constraint is Tangled Rope (coordination through doctrine + extraction of authority through fragmentation). If side effect, the constraint becomes harder to classify — it would be Rope with very high suppression, or a Scaffold that hasn''t yet achieved its sunset. This omega affects whether the reformation''s leadership is culpable or merely following doctrinal logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_side_effect, conceptual, 'Whether denominational fragmentation is a deliberate institutional strategy or a side effect of genuine theological conviction').

omega_variable(
    theological_vs_sibling_reading_causal_primacy,
    'Which reading''s causal mechanism is primary: theological incompatibility (this reading), political sovereignty pressure (political_realignment_reading), or printing technology diffusion (technological_mediation_reading)? Can the reformation be explained adequately through any single reading, or does it require all three?',
    'Comparative historical analysis: regions where theology was incompatible but political sovereignty pressure was absent (Italian city-states pre-1517) did not fragment along theological lines. Regions where printing existed but political fragmentation was low (Spanish and Portuguese empire) maintained religious unity. This suggests all three mechanisms were necessary — none alone suffices.',
    'If all three readings are necessary, the theological reading captures an important causal component but not the whole story. The constraint''s classification depends on which mechanism is defined as the primary constraint vs. which are background conditions. This omega documents that the theological reading is one perspective on a multiply-determined historical event, not a complete explanation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_vs_sibling_reading_causal_primacy, conceptual, 'The relationship between this reading and its sibling readings in explaining reformation causation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1490, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1490, reformation_composite__theological_fragmentation_reading, theater_ratio, 1490, 0.05).
narrative_ontology:measurement(refo_tr_t1520, reformation_composite__theological_fragmentation_reading, theater_ratio, 1520, 0.18).
narrative_ontology:measurement(refo_tr_t1550, reformation_composite__theological_fragmentation_reading, theater_ratio, 1550, 0.32).
narrative_ontology:measurement(refo_tr_t1580, reformation_composite__theological_fragmentation_reading, theater_ratio, 1580, 0.4).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__theological_fragmentation_reading, theater_ratio, 1618, 0.41).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__theological_fragmentation_reading, theater_ratio, 1648, 0.41).

% Extraction over time
narrative_ontology:measurement(refo_be_t1490, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1490, 0.15).
narrative_ontology:measurement(refo_be_t1520, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1520, 0.38).
narrative_ontology:measurement(refo_be_t1550, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1550, 0.56).
narrative_ontology:measurement(refo_be_t1580, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1580, 0.64).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1618, 0.68).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1648, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1490, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1490, 0.25).
narrative_ontology:measurement(refo_su_t1520, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1520, 0.35).
narrative_ontology:measurement(refo_su_t1550, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1550, 0.48).
narrative_ontology:measurement(refo_su_t1580, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1580, 0.51).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1618, 0.52).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1648, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_composite__theological_fragmentation_reading, 0.12).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% The reformation_composite kernel admits three distinct analytical readings. This constraint instantiates the theological_fragmentation_reading, treating doctrinal incompatibility as the primary causal mechanism. The political_realignment_reading treats sovereignty assertion as primary; the technological_mediation_reading treats printing diffusion as primary. All three are valid accounts of aspects of the historical Reformation. Each is authored as a separate constraint with its own ε, beneficiary/victim structure, and temporal dynamics. The three readings form a constraint family linked via affects_constraints: theological doctrine creates categories that nation-states can leverage (theology → politics); printing technology distributes all three types of arguments (technology → theology and politics); political fragmentation creates incentives for theological distinctiveness (politics → theology). The network expresses these causal influences without collapsing the readings into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
