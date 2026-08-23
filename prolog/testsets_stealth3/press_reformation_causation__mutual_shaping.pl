% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Print-Reformation Co-Evolution Frontier (Mutual-Shaping Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Between Mainz's first Bibles and Trent's book rules, printing and
 *   religious reform co-evolved. The press opened an affordance space (cheap
 *   standardized duplication, compact quarto formats, vernacular typography,
 *   fair-ground distribution); reform actors exploited it (Luther's pamphlet
 *   avalanche after 1517, vernacular Testaments, sermon postils, polemical
 *   serials); and that exploitation fed back into how printing itself
 *   developed (genre formation, edition sizing, specialization, and
 *   ultimately the regulatory counter-machinery of police ordinances, the
 *   Index, and licensed monopolies). This file instantiates ONE reading of
 *   the kernel press_reformation_causation: the mutual-shaping reading, whose
 *   standing arrangement under contest is the co-evolutionary frontier
 *   itself, and whose epsilon is authored for that referent by this reading's
 *   own lights. The sibling readings (technological_determinism,
 *   strategic_deployment) are separate constraint files with separate epsilon
 *   values; they are not folded into this story. The claim/metric gap is
 *   deliberate: the reading CLAIMS scaffold (a transitional enabling
 *   structure with a historical sunset) while the metrics are authored
 *   descriptively of the arrangement's actual operation; the engine computes
 *   each seat's classification from the structural data.
 *
 * KEY AGENTS:
 *   - protestant_reform_movements: primary beneficiary (organized/identity_locked) - exploited the press affordances; their usage patterns reshaped print development
 *   - commercial_printing_trades: dual-positioned beneficiary-and-cost-bearer (organized/constrained) - captured the monetary flow; bore capital and prosecution risk
 *   - vernacular_lay_readership: beneficiary (moderate/constrained) - gained vernacular access; demand steered output
 *   - ecclesiastical_doctrinal_establishments: primary cost-bearer turned regulator (institutional/trapped) - bore loss of communicative monopoly; their counter-machinery shaped print in turn
 *   - urban_scribal_workshops: cost-bearer (moderate/constrained) - bore displacement of manuscript production
 *   - imperial_and_municipal_print_regulators: agenda-setter by interval end (institutional/constrained) - administered the closure that ended the frontier
 *   - book_history_scholarship: analytical observer - sees the full feedback structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.34).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.48).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.34).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Print-Reformation Co-Evolution Frontier (Mutual-Shaping Reading)").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, '7a528dfe-3d43-44a2-9353-7193fc6a073d').
narrative_ontology:cs_kernel_codification('7a528dfe-3d43-44a2-9353-7193fc6a073d', distributed).
narrative_ontology:cs_authority_grounding('7a528dfe-3d43-44a2-9353-7193fc6a073d', expertise).
narrative_ontology:cs_interpretation_layer_present('7a528dfe-3d43-44a2-9353-7193fc6a073d').
narrative_ontology:cs_reading_relation('7a528dfe-3d43-44a2-9353-7193fc6a073d', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('7a528dfe-3d43-44a2-9353-7193fc6a073d', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('7a528dfe-3d43-44a2-9353-7193fc6a073d', foundational, media_agency_coconstitution).
narrative_ontology:cs_axiom_status(media_agency_coconstitution, holdable).
narrative_ontology:cs_axiom_grounding('7a528dfe-3d43-44a2-9353-7193fc6a073d', media_agency_coconstitution, empirically_contingent).
narrative_ontology:cs_axiom('7a528dfe-3d43-44a2-9353-7193fc6a073d', foundational, enabling_structure_is_transitional).
narrative_ontology:cs_axiom_status(enabling_structure_is_transitional, holdable).
narrative_ontology:cs_axiom_grounding('7a528dfe-3d43-44a2-9353-7193fc6a073d', enabling_structure_is_transitional, empirically_contingent).
narrative_ontology:cs_reference_frame('7a528dfe-3d43-44a2-9353-7193fc6a073d', coevolutionary_press_reform_coupling).
narrative_ontology:cs_drift_state('7a528dfe-3d43-44a2-9353-7193fc6a073d', postrevisionist_scholarship, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('7a528dfe-3d43-44a2-9353-7193fc6a073d', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, protestant_reform_movements).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, commercial_printing_trades).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, vernacular_lay_readership).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, ecclesiastical_doctrinal_establishments).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, urban_scribal_workshops).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, commercial_printing_trades).
narrative_ontology:constraint_vindicates(press_reformation_causation__mutual_shaping, media_agency_coevolution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispersed evangelical authors, preachers, and movement organizers across the German lands, Swiss cantons, and beyond. They wrote treatises, translated scripture, and supplied a steady stream of short, cheap works timed to controversy; printers sought them out because their names sold editions. Their program identified vernacular scripture as its very substance, so their public existence became inseparable from the medium that carried it; abandoning print would have meant recasting the whole movement as clandestine cells.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, protestant_reform_movements, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, protestant_reform_movements, agenda_setter).

% Master printers and publisher-booksellers in Basel, Strasbourg, Wittenberg, Antwerp, Geneva, and the Frankfurt fair middlemen who moved their stock. They financed speculative editions on borrowed capital, rode the booming market for religious controversy and vernacular Bibles, and absorbed the downside: confiscated sheets, revoked privileges, and occasional prosecution of sellers of prohibited titles. A minority relocated presses across borders when local bans bit; most were anchored by type, equipment, credit, and trained hands.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, commercial_printing_trades, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, commercial_printing_trades, payer).

% Urban artisans, merchants, schoolmasters, and parish clergy who bought or borrowed cheap quartos, broadsheets, hymnals, and vernacular New Testaments. Print gave them direct access to texts previously mediated through clergy, at prices within reach of the propertied classes. Rural and illiterate populations met this material secondhand, through read-aloud gatherings and pulpit repetition. Their purchasing patterns steered what was printed next.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, vernacular_lay_readership, beneficiary,
    moderate, biographical, constrained, regional).

% The Roman Curia, bishops, university theology faculties, and after 1559 the Congregation of the Index. For decades they watched their control over doctrinal publication erode as unauthorized editions circulated faster than warnings could travel. They answered with condemnations, burnings, privilege negotiations, and finally standing review machinery: the Pauline Index, Trent's rules on books, territorial imprimaturs. By interval end they administered, inside their own jurisdictions, the licensed order that replaced the open market they had lost; their authority was constituted through communication, so withdrawing from the field altogether was not available to them.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, ecclesiastical_doctrinal_establishments, payer,
    institutional, civilizational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, ecclesiastical_doctrinal_establishments, agenda_setter).

% Scriptoria, stationers, and copyist workshops that had supplied manuscript books and documents. Demand for hand-copied texts collapsed as printed editions undercut them on price and speed. Some workshops pivoted to illumination, ruling, and finishing work for printers, or to administrative copying; others contracted steadily across the century.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, urban_scribal_workshops, payer,
    moderate, biographical, constrained, regional).

% Imperial diets drafting police ordinances, territorial princes issuing mandates, and city councils licensing presses. They spent the interval oscillating between protecting a lucrative trade and answering complaints about sedition and disorder, converging after mid-century on pre-publication licensing, printer sureties, and cross-border enforcement cooperation. Their charters and ordinances mark when and where the open publishing environment formally ends.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, imperial_and_municipal_print_regulators, agenda_setter,
    institutional, generational, constrained, continental).

% Historians of the book and of the Reformation working from colophons, edition runs, inventories, and institutional archives. They reconstruct, edition by edition, what was printed where, in which languages and formats, and correlate those patterns with reform activity and regulatory change; they see the whole feedback circuit and both of its termini.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, book_history_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, commercial_printing_trades).
narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled a geographically scattered religious movement to act in concert without territorial sovereignty: identical cheap editions synchronized doctrine, liturgy, and polemic across dozens of jurisdictions faster than any chancery could answer. On the commercial side, predictable movement demand let printers match speculative capital to guaranteed audiences.
% TRANSFER_FUNCTION: Moved doctrinal text and interpretive authority from clerical mediation toward lay readers; moved coin from purchasers to printers, authors, and paper suppliers; moved reputational standing among reformers through citation, translation, and piracy networks; and moved punitive attention from pulpits onto presses and peddlers in the form of bans, seizures, and prosecutions.
% ABSENT_VOICES: The illiterate rural majority, who met print only as read-aloud hearsay and had no seat in deciding what was published; women, largely excluded from shop ownership apart from a minority of widow-run presses; and radical dissenters (Anabaptist and spiritualist writers), whose works every establishment, Protestant and Catholic alike, squeezed hardest, leaving them the least-accessible voice in the new public sphere.
% DISAPPEARANCE_RATIONALE: By interval end, confessional identities, catechetical schooling, vernacular Bible reading, hymnody, and the structure of the book trade itself all presupposed the fifty-year exchange between presses and reform movements. Overnight removal at 1600 would strand established churches without their printed formularies, unwind standardized worship across territories, and collapse a commercial sector built on controversy-driven demand: the communicative order rearranges around the hole.
% FOUNDING_PROBLEM: No founder designed the arrangement. It condensed around two problems that fused in the 1517-1520s: how a reform movement with no army or treasury could broadcast and synchronize its program across hostile jurisdictions, and how an expensive new duplicating technology could secure reliable mass demand. Cheap topical quartos answered both at once.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: imperial diet and city-council records document the disorder contemporaries wanted managed, from administrative seats indifferent to reform's success; Catholic controversialists such as Johannes Cochlaeus and Johannes Eck left detailed testimony of print-driven propagation they opposed; and the licensing apparatus itself (Stationers' charter 1557, Pauline Index 1559, Trent's ten rules on books 1564) exists only because authorities across confessions judged the unmanaged arrangement untenable. No surviving source outside the movement argues the open frontier was sustainable as it stood.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.34 at interval end) because the arrangement's costs to its participants were real but bounded: speculative capital tied up in editions, prosecution exposure under the Worms ban (the publisher Hans Hergot was executed in 1527), and, after mid-century, licensing compliance overhead. The series peaks around 1550, when war disruption and tightening mandates raised the risk premium, then settles as regulated equilibrium replaced hazardous openness. Suppression (0.48 end-state) is authored as a RAW structural intensity and is not scaled by anything in authoring - only extractiveness gets scaled downstream, by directionality and scope. Its trajectory rises steeply through the closure decades: burnings of Luther's works (1520-21), the Edict of Worms, imperial police ordinances (1530, 1548), the Index (1559), Trent's book rules (1563-64), the Stationers' monopoly (1557) - then eases slightly as coercion normalized into routine administration. Note the semantic care: the arrangement never enforced ITSELF (opportunity carried it; requires_active_enforcement is false); the measured suppression is the coercive field its participants increasingly operated inside, which this reading counts as the arrangement's operative condition. Theater stays low throughout (0.21 end-state): licensing formalities and ceremonial burnings added ritual in the late interval, but the arrangement remained functional to the end; nothing here approaches proxy-goal substitution. Accessibility_collapse is 0.40 because alternatives never collapsed: preaching remained the dominant vehicle of religious communication for the entire interval, manuscript devotion survived in parallel, and oral/memory networks carried most of the population. Resistance is 0.62 - systematic, organized, and partly successful (closure happened). Identity-lock dynamics: the reform movements' exit option is identity_locked through ideological fusion - sola scriptura made vernacular print constitutive of the program, so leaving the medium meant dissolving the movement's public form; a counterfactual in which that doctrinal frame broke (acceptance of purely oral transmission) would move the seat toward mobile and soften the whole feedback loop. The temporal series run on ONE shared 25-year grid (seven points, every tracked metric authored at every point), so no metric borrows another's endpoint. The trajectory is monotone-drifting rather than cyclical; no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the ecclesiastical establishment's position the same half-century reads as dispossession: its communicative authority was dismantled by a machine it did not control, and its eventual regulatory victory came only after the decisive damage. From the reform movements' position the same arrangement reads as providential enablement - nearly free reach they could never have purchased. The printing trade sits genuinely dual: largest monetary recipient, largest uninsured risk-bearer. Same-level lateral differentiation is sharp here: commercial printers and urban scribal workshops held comparable trade power, guild-like organization, and urban embedding, yet sit at opposite directionalities - differentiated entirely by their relationship to the new medium, not by global standing. Likewise two institutional actors diverge: the ecclesiastical establishments (trapped, paying) versus the imperial and municipal regulators (agenda-setting by interval end) - same nominal estate of governing authority, opposite trajectories, because jurisdictional position determined who administered the closure and who was closed upon.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive derivation and no overrides are needed. Protestant reform movements: declared beneficiary with identity_locked exit - derivation places them near the full-beneficiary end (their costs, though real, were chosen and constitutive). Vernacular lay readership: beneficiary with constrained exit - low-to-mid directionality; their benefit was broad but shallow for the illiterate majority (see the lay_readership_benefit_depth omega). Commercial printing trades: dual-positioned (beneficiary + payer) with constrained exit - derivation lands them near symmetric, which matches the historical ledger of fortunes made and ruined. Urban scribal workshops: declared victim with constrained exit - high directionality toward target. Ecclesiastical establishments: declared victim, trapped - nearest the full-target end; they bore the arrangement's largest uncompensated losses and their countermeasures are the feedback term the mutual-shaping reading insists on. Imperial and municipal regulators: agenda_setter seat; they receive order and revenue from the successor licensing regime rather than from the frontier arrangement, so their effective position is administrative rather than extractive. Scope amplification applies through the engine: the arrangement's continental scope raises verification difficulty and hence effective extraction for targeted seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this arrangement as a scaffold with a genuine historical sunset prevents two symmetrical errors. Reading it as a permanent coordination fixture (the determinist temptation) mistakes a transitional enabling structure for steady-state infrastructure; reading it as pure predation (the extraction-only temptation) misses that the frontier solved a real collective-action problem for a movement that had no other channel. The sunset was structural rather than drafted: police ordinances, the Index, Trent's book rules, and national licensing charters are the arrangement's termination provisions, authored by the very contest it generated. The R5 battery confirms completion rather than zombification: founding status is contested (the movement-building problem was absorbed into durable confessional institutions while communicative-order contention lived on past 1600), and the measured theater ratio (0.21) sits far below the threshold at which a mandate-outlived-function signature would appear. Nothing here persists by inertia; the arrangement ended, which is what a scaffold is for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates one reading (mutual_shaping) of the kernel press_reformation_causation; what fixes the mutual-shaping instantiation against its siblings, technological_determinism and strategic_deployment?',
    'Chronological test on the print record: if edition runs and format innovations systematically FOLLOW reform-demand spikes and regulatory shocks (demand- and conflict-led development), mutual shaping is fixed; if diffusion tracks technical capacity independent of agency decisions, determinism gains; if adoption timing reduces to identifiable elite strategies, deployment gains.',
    'Resolution reallocates the epsilon referent and the family classifications: the determinism reading computes as a natural-force claim, deployment as an instrument claim, mutual shaping as a transitional enabling structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading the evidence fixes within the contested causation kernel.').

omega_variable(
    suppression_attribution_ambiguity,
    'Is the late-interval suppression rise a property of the co-evolutionary arrangement itself, or of the successor licensing machinery pressing on its remnants?',
    'Compare coercion intensity across territories where closure failed or arrived late versus where it succeeded: if coercion tracks regulator capacity rather than the arrangement''s vitality, attribution runs to the exogenous successor regime.',
    'Internal attribution raises the arrangement''s own coercive profile toward hybrid-coordination territory; external attribution leaves the transitional-support reading intact with suppression belonging to the successor order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_attribution_ambiguity, empirical, 'Whether measured late-period coercion belongs to the arrangement or to the regimes that replaced it.').

omega_variable(
    arrangement_boundary_ambiguity,
    'Where does the co-evolutionary arrangement actually end - at formal closure (police ordinances, Index, Trent''s book rules), or does it persist latently wherever licensing stayed porous?',
    'Trace post-1564 print innovation inside nominally closed jurisdictions: whether religious-use feedback still visibly steered development (smuggled editions, evasive imprints, format experiments driven by devotional demand).',
    'If latent persistence is real, the sunset declaration dates early and the end-state epsilon shifts upward; if closure held, the transitional reading stands with a clean terminus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arrangement_boundary_ambiguity, conceptual, 'Boundary of the standing arrangement: formal closure versus porous-latent continuation.').

omega_variable(
    lay_readership_benefit_depth,
    'How far did vernacular print access actually reach beyond urban propertied literates?',
    'Probate inventories, library catalogues, and edition-run economics against wage series: estimate the share of households able to buy rather than merely hear printed religion secondhand.',
    'Shallow reach moves the readership seat toward symmetric positionality and thins the breadth of the coordination-function claim; deep reach sustains broad-based benefit and low reader-side directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_readership_benefit_depth, empirical, 'Depth and breadth of the lay benefit that anchors one beneficiary seat.').

omega_variable(
    cs_framing_underdetermination,
    'Is expert adjudication (authority_grounding: expertise) the right frame for this kernel''s authority structure, or is authority fully distributed across competing scholarly camps with no adjudicator at all?',
    'Observe whether peer review and monograph consensus actually discipline rival causal claims over time (expertise frame holds) or whether camps persist incommensurably across generations (distributed frame holds).',
    'Under the distributed frame, interpretation_layer_present loses validity and the drift reading loses its adjudicated anchor; the kernel''s authority classification changes accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of who, if anyone, adjudicates this kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__mutual_shaping, theater_ratio, 1450, 0.04).
narrative_ontology:measurement_basis(pres_tr_t1450, observed).
narrative_ontology:measurement(pres_tr_t1475, press_reformation_causation__mutual_shaping, theater_ratio, 1475, 0.06).
narrative_ontology:measurement_basis(pres_tr_t1475, observed).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__mutual_shaping, theater_ratio, 1500, 0.08).
narrative_ontology:measurement_basis(pres_tr_t1500, observed).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causation__mutual_shaping, theater_ratio, 1525, 0.07).
narrative_ontology:measurement_basis(pres_tr_t1525, observed).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__mutual_shaping, theater_ratio, 1550, 0.13).
narrative_ontology:measurement_basis(pres_tr_t1550, observed).
narrative_ontology:measurement(pres_tr_t1575, press_reformation_causation__mutual_shaping, theater_ratio, 1575, 0.18).
narrative_ontology:measurement_basis(pres_tr_t1575, observed).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__mutual_shaping, theater_ratio, 1600, 0.21).
narrative_ontology:measurement_basis(pres_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement_basis(pres_be_t1450, observed).
narrative_ontology:measurement(pres_be_t1475, press_reformation_causation__mutual_shaping, base_extractiveness, 1475, 0.18).
narrative_ontology:measurement_basis(pres_be_t1475, observed).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__mutual_shaping, base_extractiveness, 1500, 0.22).
narrative_ontology:measurement_basis(pres_be_t1500, observed).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causation__mutual_shaping, base_extractiveness, 1525, 0.36).
narrative_ontology:measurement_basis(pres_be_t1525, observed).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__mutual_shaping, base_extractiveness, 1550, 0.41).
narrative_ontology:measurement_basis(pres_be_t1550, observed).
narrative_ontology:measurement(pres_be_t1575, press_reformation_causation__mutual_shaping, base_extractiveness, 1575, 0.37).
narrative_ontology:measurement_basis(pres_be_t1575, observed).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__mutual_shaping, base_extractiveness, 1600, 0.34).
narrative_ontology:measurement_basis(pres_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__mutual_shaping, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement_basis(pres_su_t1450, observed).
narrative_ontology:measurement(pres_su_t1475, press_reformation_causation__mutual_shaping, suppression_requirement, 1475, 0.12).
narrative_ontology:measurement_basis(pres_su_t1475, observed).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__mutual_shaping, suppression_requirement, 1500, 0.16).
narrative_ontology:measurement_basis(pres_su_t1500, observed).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causation__mutual_shaping, suppression_requirement, 1525, 0.31).
narrative_ontology:measurement_basis(pres_su_t1525, observed).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__mutual_shaping, suppression_requirement, 1550, 0.44).
narrative_ontology:measurement_basis(pres_su_t1550, observed).
narrative_ontology:measurement(pres_su_t1575, press_reformation_causation__mutual_shaping, suppression_requirement, 1575, 0.51).
narrative_ontology:measurement_basis(pres_su_t1575, observed).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__mutual_shaping, suppression_requirement, 1600, 0.48).
narrative_ontology:measurement_basis(pres_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, identity_coordination).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).

% DUAL FORMULATION NOTE:
% The colloquial label 'the printing press caused the Reformation' decomposes, per the epsilon-invariance principle, into three structurally distinct causal constraints sharing the kernel press_reformation_causation: technological_determinism (unilateral necessity; referent: print's effect as irresistible force), strategic_deployment (neutral capacity deliberately aimed; referent: the instrumental-use arrangement), and this file, mutual_shaping (bidirectional co-evolution; referent: the enabling frontier arrangement itself, which this reading treats as transitional with a structural sunset). Each story carries its own epsilon, beneficiary structure, and classification; they are linked as one family here, and this reading sits causally downstream of neither sibling - it contests the causal geometry both assume.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
