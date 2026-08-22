% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Technology-Reformation Co-Constitution: Press Enables, Reformers Shape
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The co-constitutional reading treats technology (the printing press) and
 *   social actors (reformers, Church authorities, printers, secular rulers)
 *   as mutually constitutive forces whose interaction produced the
 *   Reformation. The press was not inevitable—it emerged from prior
 *   technical, economic, and intellectual developments—and the Reformation
 *   was not automatic from the press's existence. Rather, reformers actively
 *   shaped what the press produced through patronage, text selection, and
 *   editorial direction; simultaneously, the press enabled reformers to scale
 *   their reach beyond what oral networks could achieve. The constraint
 *   models this bidirectional causality: reformers benefited from press
 *   affordances (enabling distribution and standardization), but the press
 *   itself was reshaped by their demands and by Church authorities' attempts
 *   to suppress it. The extractiveness measurement rises steadily from 1450
 *   to 1510 (as the press becomes a contested resource) then plateaus around
 *   0.48, indicating mature tension between coordination benefits and
 *   institutional conflict. Theater ratio rises through the same period as
 *   Church authorities invest in suppression theater (licensing, Indexes,
 *   theological argument) that cannot actually halt the technology's
 *   adoption.
 *
 * KEY AGENTS:
 *   - Reformers (Luther, Zwingli, Calvin, radicals): Agenda-setters who directed printer patronage and shaped what texts were printed; beneficiaries of press affordances but constrained by Church authority.
 *   - Printing technology and printers: The coordination mechanism and the economic actors who captured profit; shaped by demand from reformers but not determined by their intentions.
 *   - Church authorities: Payers bearing the cost of lost interpretive monopoly; unable to exit (bound by doctrine) or to suppress completely (technology too decentralized).
 *   - Manuscript producers (scribes, copyists): Payers bearing displacement costs as printing replaced hand-copying.
 *   - Secular rulers: Agenda-setters who later deployed printing for religious and political authority; beneficiaries of the coordination mechanism.
 *   - Illiterate populations: Payers facing exclusion from text-centered authority systems.
 *   - Technological determinists (analytical observers): Representatives of the competing reading that treats the press as autonomous cause.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.48).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.52).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Technology-Reformation Co-Constitution: Press Enables, Reformers Shape").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__co_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, 'dfd4f363-e866-4c1b-bd72-22863fb52998').
narrative_ontology:cs_kernel_codification('dfd4f363-e866-4c1b-bd72-22863fb52998', fixed_text).
narrative_ontology:cs_authority_grounding('dfd4f363-e866-4c1b-bd72-22863fb52998', lineage).
narrative_ontology:cs_interpretation_layer_present('dfd4f363-e866-4c1b-bd72-22863fb52998').
narrative_ontology:cs_reading_relation('dfd4f363-e866-4c1b-bd72-22863fb52998', technology_reformation_causality__technological_determinism_reading, influences).
narrative_ontology:cs_reading_relation('dfd4f363-e866-4c1b-bd72-22863fb52998', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_axiom('dfd4f363-e866-4c1b-bd72-22863fb52998', foundational, bidirectional_causality_in_technological_change).
narrative_ontology:cs_axiom_status(bidirectional_causality_in_technological_change, holdable).
narrative_ontology:cs_axiom_grounding('dfd4f363-e866-4c1b-bd72-22863fb52998', bidirectional_causality_in_technological_change, empirically_contingent).
narrative_ontology:cs_axiom('dfd4f363-e866-4c1b-bd72-22863fb52998', foundational, technological_affordance_requires_social_deployment).
narrative_ontology:cs_axiom_status(technological_affordance_requires_social_deployment, holdable).
narrative_ontology:cs_axiom_grounding('dfd4f363-e866-4c1b-bd72-22863fb52998', technological_affordance_requires_social_deployment, empirically_contingent).
narrative_ontology:cs_reference_frame('dfd4f363-e866-4c1b-bd72-22863fb52998', manuscript_based_ecclesiastical_authority).
narrative_ontology:cs_drift_state('dfd4f363-e866-4c1b-bd72-22863fb52998', printing_mediated_reformation_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dfd4f363-e866-4c1b-bd72-22863fb52998', '2026-06-12T14:37:00Z').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reformers_and_allied_scholars).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, church_authorities_defending_monopoly).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, scribal_manuscript_producers).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, illiterate_populations_excluded_from_written_vernacular).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, printing_entrepreneurs_and_merchants).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, secular_authorities_and_rulers).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, illiterate_populations).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, bidirectional_causality_in_technological_change).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, technological_affordance_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Intellectual and theological reformers—Luther, Zwingli, Calvin, radical reformers—identified the press as an enabling technology for their agenda: vernacular scripture, lay literacy, direct access to textual authority. They actively shaped what the press produced by selecting texts to print, directing printer patronage, writing prefaces and editorial frames, and mobilizing distribution networks. Their exit from Church authority was constrained by ecclesiastical power and doctrine, but the press enabled them to bypass the monopoly on scriptural interpretation.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reformers_and_allied_scholars, agenda_setter,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, reformers_and_allied_scholars, beneficiary).

% The press itself (and the economic interests of printers) emerged as a coordination mechanism: it solved the problem of mass-producing identical texts cheaper than scribal reproduction, but its affordances were shaped by how reformers, Church authorities, and merchants demanded it be used. Printers extracted profit and social standing by printing what was demanded; the technology enabled but did not determine what got printed.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printing_technology_and_printers, beneficiary,
    analytical, biographical, analytical, continental).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__co_constitution_reading, printing_technology_and_printers).

% Ecclesiastical authorities bore the cost of Reformation via erosion of interpretive monopoly and institutional authority. Their exit options were limited by doctrine and institutional inertia: they could suppress printing (expensive, incomplete, met resistance), license it (loss of control), or accommodate it (doctrinal concession). They initially tried suppression and licensing; accommodation came only after centuries of conflict.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, church_authorities_defending_monopoly, payer,
    powerful, generational, constrained, continental).

% Scribal copyists and manuscript houses lost market share and social standing as printing replaced hand-copying for mass-produced texts. Their exit was relatively mobile—some retrained as typesetters or editors—but their medieval role became obsolete within two generations. They bore the displacement cost of technological transition.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, scribal_manuscript_producers, payer,
    moderate, biographical, mobile, continental).

% Populations without literacy were excluded from the written vernacular explosion the Reformation enabled. While printed materials could be read aloud, the shift from oral culture (sermons, church ritual) to text-centered authority created a literacy divide. Illiterate populations depended on intermediaries (clergy, educated laity) for access to meaning.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, illiterate_populations, payer,
    powerless, biographical, trapped, continental).

% Merchants and entrepreneurs who invested in printing infrastructure captured value from the technology's affordances. Their profit depended on demand from reformers, universities, merchants, and—eventually—governments. They benefited from the technology but were not its primary shapers; they responded to market demand reformers and other actors created.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printing_entrepreneurs_and_merchants, beneficiary,
    moderate, biographical, mobile, continental).

% Princes and rulers who adopted Reformation used printing as an enforcement mechanism for religious and political authority. They shaped what printers produced through licensing, subsidy, and suppression. Their agenda-setting power was exercised through the affordances the technology provided—they could not have centralized religious authority or literacy policy without it, but the technology alone did not determine their choices.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, secular_authorities_and_rulers, agenda_setter,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, secular_authorities_and_rulers, beneficiary).

% The analytical seat observing whether technology or human agency is the primary driver of historical change. A determinist reading treats the printing press as the autonomous cause and reformers as secondary executors; a co-constitutional reading treats press and reformers as mutually constitutive.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, technological_determinists, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__co_constitution_reading, technological_determinists).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__co_constitution_reading, reformers_and_allied_scholars).
narrative_ontology:fixing_cost_class(technology_reformation_causality__co_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of mass-producing, distributing, and standardizing religious texts and scholarly work at scale; enables coordination of beliefs and knowledge distribution across dispersed populations and languages that manuscript reproduction could not achieve; coordinates the interests of reformers, printers, and lay readers around text-centered authority.
% TRANSFER_FUNCTION: Transfers interpretive authority from the Church's monopoly on scriptural gatekeeping to networked reformers, printers, and lay readers through vernacular printed scripture. Moves economic value from manuscript producers (scribes, copyists) to printers and merchant networks. Moves institutional authority from centralized ecclesiastical structures to dispersed Reformed churches and secular rulers. Moves literacy-based status from the clergy to educated laity.
% ABSENT_VOICES: Non-literate populations who are excluded from the benefits of text-centered authority and depend on intermediaries (clergy, educated laity) for access to meaning. Conservative theological positions within the Church that valued living oral tradition and hierarchical interpretation over textual standardization. Manuscript producers and scribal workshops who lost market share and social standing. Regional and peripheral actors outside major printing centers (Ottoman territories, Eastern Orthodoxy, peripheral monasteries) whose access to printing was mediated by distant commercial and political powers.
% DISAPPEARANCE_RATIONALE: If the printing press had not been invented or had disappeared by 1550, the Reformation would have been fundamentally constrained: reform movements would have remained local, knowledge dissemination would have depended on smuggled manuscripts and oral networks (slower, harder to standardize, more vulnerable to suppression), Church authority over doctrine would not have faced the same existential challenge to its monopoly, literacy would have remained tied to scribal and clerical networks (much lower rates), and the eventual secularization of knowledge and vernacular literacy would have occurred on a timescale of centuries if at all.
% FOUNDING_PROBLEM: By the 15th century, intellectual and theological pressure against the Church's interpretive monopoly (from humanist scholars, reformist preachers, and lay piety movements) was mounting. Simultaneously, demand for access to scripture in vernacular languages exceeded what scribal reproduction could supply, and the Church's control of manuscript production gave it de facto gatekeeping power over textual access. The constraint models how reformers, printers, and Church authorities negotiated control over the coordination mechanism (mass-produced standardized texts) that could break the Church's monopoly while enabling new forms of religious and intellectual authority.
% FOUNDING_PROBLEM_CORROBORATION: Reformation historians (MacCulloch, Gregory, Ozment, Pettegree) outside the determinist camp attest that intellectual pressure on Church authority was mounting prior to the press's invention (the founders of reform movements predate widespread printing). Historians of the book (Eisenstein, Moll, Suarez) document that reformers strategically deployed the press, selecting texts and sponsoring editions. Church suppression records (licensing edicts, heresy trials, Index creation) show authorities recognized the press as a threat to their authority—a recognition that would be unnecessary if the technology were autonomous rather than deployed by agents with contrary interests. No credible historian now holds that the press operated autonomously; the scholarly consensus is that reformer agency, technological affordances, Church resistance, and economic incentives were all necessary and mutually constitutive.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading emphasizes bidirectional causality, so extractiveness rises with the interaction term between technological affordances and reformer agency. Early (1450–1475), extractiveness is low (0.15–0.28) because the press is novel, demand is uncertain, and reformers have not yet identified it as a strategic asset. Mid-period (1475–1510), extractiveness accelerates (0.28–0.47) as reformers actively patronize printing of vernacular scripture and Church authorities recognize the threat and escalate suppression. Late period (1510–1550), extractiveness plateaus (0.47–0.48) because the technology and reformer agenda are now locked in; further escalation is costlier than accommodation for Church authorities. Suppression requirement tracks extractiveness closely, indicating that persistence depends on continued active enforcement (licensing, Index, censorship). Theater ratio (performative activity that does not halt the underlying process) rises through the same curve, suggesting that as Church suppression machinery grows, an increasing share of its activity is theater—the press is too distributed to suppress, so authorities invest in appearances of control (licensing edicts that are violated, heresy trials that do not prevent printing) rather than functional suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer seat: the press is an enabling technology that I strategically deployed; my agency and the technology's affordances co-produced the Reformation. From the Church authority seat: the press is a destabilizing force that undermined my institutional authority despite my suppression efforts; technology and reform agency are two sides of the same threat. From the printer seat: I responded to market demand created by reformers and Church conflict; I had no predetermined ideological commitment. From the determinist observer seat: the technology was the primary driver; human choices merely executed what the press's logic required. The engine computes these divergent classifications from the positional data (power, exit, beneficiary/victim status); they are not pre-adjudicated by the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers are beneficiaries (collects interpretive authority, can reach audiences at scale) with constrained exit (Church doctrine and political pressure limit their alternatives to printing or silence). They are agenda-setters (actively direct printer patronage). Their directionality is composite: they benefit from the constraint but also shape it, so d is moderate (around 0.35–0.40) rather than near-full-beneficiary. Church authorities are victims (lose monopoly, bear suppression costs, face institutional erosion) with constrained exit (doctrine and institutional inertia); they are payers but have moderate power (they could theoretically accommodate or exit; they choose costly resistance). Their d is moderate (around 0.55–0.65). Printers are beneficiaries (profit from demand) but not agenda-setters (they respond to market signals); their d is moderate-beneficiary (around 0.25–0.30). Manuscript producers are victims (displaced) with mobile exit (they can retrain); their d is moderate-target (around 0.60). Secular rulers are agenda-setters (use printing for authority) with mobile exit (they adopt printing strategically); their d is low (around 0.15–0.20). The constraint computes as Tangled Rope at most seats because it coordinates text production and distribution (genuine coordination function) while extracting from Church authorities and manuscript producers who cannot easily exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the false dichotomy between technological determinism (the press caused the Reformation; human agency is secondary) and pure agency (reformers caused the Reformation; technology is merely a tool). The mandatrophy question is: Did the Reformation's founding problem (challenge to Church authority over doctrine) persist into the constraint's mature phase (1550), or did the constraint persist because the founding problem had been solved (world_unchanged) or forgotten? The corroboration is clear: historians outside the determinist camp attest that secular rulers had adopted Reformation principles, vernacular literacy was established, Church authority over doctrine was permanently fragmented. The founding problem (challenge to interpretive monopoly) is LIVE at 1550 in the sense that institutional conflict continues, but it is CONTESTED whether the problem remains urgent or whether the constraint has become theater (accommodation is the norm; suppression is increasingly performative). The coercion grid at 1550 shows organizational and structural resistance still high (0.72, 0.68), suggesting ongoing institutional conflict; individual resistance is lower (0.38), suggesting accommodation at the lay level. Theater ratio at 0.31 is moderate, not the high ratio characteristic of Piton (0.6+), so the constraint has not yet degraded into pure inertia, but the trend is visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    determinism_vs_coevolution_causal_structure,
    'Is the Reformation''s causality asymmetrical (technology or agency dominates) or bidirectional (technology and agency mutually constitutive)?',
    'Counterfactual history: (a) If the printing press had never existed, would the Reformation have occurred via manuscript distribution and oral networks? (b) If reformers had not actively patronized printing and shaped content, would the press have produced the same literary output? Natural experiments: instances where the technology existed but was not deployed by reform movements (Ottoman printing restrictions) or where reform movements existed without the press (medieval heresies).',
    'If causality is asymmetrical, reclassify to the determining-axis reading. If bidirectional, confirm co-constitution. If the interaction term is dominant (both axes required, neither alone sufficient), the bidirectional reading is justified and extractiveness models the tension between enablement and resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinism_vs_coevolution_causal_structure, conceptual, 'Whether causal structure is monolinear (one axis dominates) or bidirectional (mutual constitution).').

omega_variable(
    technological_affordance_vs_historical_contingency,
    'Were the printing press''s affordances (mass production, standardization, distribution at scale) structurally necessary for Reformation success, or could alternative information technologies (manuscript networks, oral preaching expansion) have achieved similar outcomes?',
    'Historical comparison: instances where reformation movements succeeded without printing (early Christian heresies, Islamic scholasticism expansion without press). Technical analysis: what specific affordances of printing were non-substitutable for the Reformation''s strategy? Could manuscript distribution networks have scaled sufficiently? Could oral networks have maintained doctrinal coherence across geography?',
    'If affordances were structurally necessary and non-substitutable, the press constraint is closer to Mountain (natural law of information distribution). If alternatives existed, the constraint is closer to Snare (Church monopoly was artificial, press was one way to break it; other ways existed). If affordances were enabling but not determining, co-constitution stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_affordance_vs_historical_contingency, empirical, 'Whether printing press affordances were structurally necessary or historically contingent substitutes existed.').

omega_variable(
    church_suppression_agency_and_accommodation_dynamics,
    'Did Church authorities'' eventual accommodation of the press (licensing, selective printing of approved texts, theological engagement) represent a genuine shift in constraints, or continued theater masking unchanged institutional resistance?',
    'Institutional analysis: Did Church authorities'' printing policies after 1550 represent functional accommodation (licensing was enforced, approved texts were genuinely defended, theological engagement was substantive) or performance (licenses were routinely violated, approved texts were marginal to actual demand, theological debate was symbolic)? Archive analysis of licensing enforcement, printer compliance rates, and actual versus approved printing volumes.',
    'If accommodation was genuine, the constraint shifted from Tangled Rope (active enforcement) toward Rope (coordinated use of press by multiple authorities). If continued theater, the constraint is drifting toward Piton (atrophied alternatives masked by performative suppression). This informs the theater_ratio trajectory and mandatrophy verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(church_suppression_agency_and_accommodation_dynamics, empirical, 'Whether Church accommodation of press licensing represented genuine constraint shift or performative continuity.').

omega_variable(
    reformer_shaping_agency_scope_and_limits,
    'To what extent did reformers'' patronage and text selection actually shape the press''s output, versus the press responding to broader economic and intellectual demand that reformers merely joined?',
    'Bibliographic and patronage analysis: correlation between reformer patronage and printing output (did texts they sponsored dominate? did volumes spike in patronized forms?). Printer correspondence and contracts: do printers'' records show reformer influence on content selection, or did reformers respond to printer offers? Control case: compare reformer-patronized printing with non-patronized printing in the same markets; do the volumes, formats, and distribution differ significantly?',
    'If reformers substantially shaped output, agenda-setter classification is justified and extractiveness rises from their agency. If reformers were responding to broader demand, their directionality shifts toward beneficiary (they collected from a technology others were already developing). If influence was mutual, co-constitution is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformer_shaping_agency_scope_and_limits, empirical, 'The extent to which reformer patronage and demand shaped printing output versus economic markets driving the technology independent of their influence.').

omega_variable(
    committer_kernel_framing_alternative_readings,
    'Within the single kernel ''technology_reformation_causality,'' do the three readings (determinism, beneficiary_agency, co_constitution) represent genuine logical alternatives, or do they describe the same constraint from different observational angles?',
    'Structural comparison: Do the readings produce different ε values for the same standing arrangement, or different standing arrangements? Do they assign different beneficiary/victim sets, or the same sets with different causal narratives? If ε differs, they are different constraints (ε-invariance principle: decompose). If ε is the same and only narrative emphasis differs, they are one constraint with observational ambiguity (a conceptual omega, not a decomposition candidate).',
    'If the readings are structurally distinct (different ε), decompose into separate constraint stories per the ε-invariance principle; link them via network.affects_constraints. If they are one constraint with competing narratives, document the narrative ambiguity in a kernel_context omega and consolidate to a single story modeling the kernel-framing debate. Current authoring assumes decomposition (three separate constraint stories); if this judgment is incorrect, merge the three readings into a single story with three competing interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_framing_alternative_readings, conceptual, 'Whether the three kernel readings are structurally distinct constraints or one constraint with competing observational framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__co_constitution_reading, theater_ratio, 1450, 0.08).
narrative_ontology:measurement_basis(tech_tr_t1450, observed).
narrative_ontology:measurement(tech_tr_t1475, technology_reformation_causality__co_constitution_reading, theater_ratio, 1475, 0.12).
narrative_ontology:measurement_basis(tech_tr_t1475, observed).
narrative_ontology:measurement(tech_tr_t1490, technology_reformation_causality__co_constitution_reading, theater_ratio, 1490, 0.18).
narrative_ontology:measurement_basis(tech_tr_t1490, observed).
narrative_ontology:measurement(tech_tr_t1510, technology_reformation_causality__co_constitution_reading, theater_ratio, 1510, 0.26).
narrative_ontology:measurement_basis(tech_tr_t1510, observed).
narrative_ontology:measurement(tech_tr_t1530, technology_reformation_causality__co_constitution_reading, theater_ratio, 1530, 0.31).
narrative_ontology:measurement_basis(tech_tr_t1530, observed).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__co_constitution_reading, theater_ratio, 1550, 0.31).
narrative_ontology:measurement_basis(tech_tr_t1550, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement_basis(tech_be_t1450, observed).
narrative_ontology:measurement(tech_be_t1475, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1475, 0.28).
narrative_ontology:measurement_basis(tech_be_t1475, observed).
narrative_ontology:measurement(tech_be_t1490, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1490, 0.38).
narrative_ontology:measurement_basis(tech_be_t1490, observed).
narrative_ontology:measurement(tech_be_t1510, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1510, 0.47).
narrative_ontology:measurement_basis(tech_be_t1510, observed).
narrative_ontology:measurement(tech_be_t1530, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1530, 0.48).
narrative_ontology:measurement_basis(tech_be_t1530, observed).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1550, 0.48).
narrative_ontology:measurement_basis(tech_be_t1550, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1450, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1450, 0.2).
narrative_ontology:measurement_basis(tech_su_t1450, observed).
narrative_ontology:measurement(tech_su_t1475, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1475, 0.32).
narrative_ontology:measurement_basis(tech_su_t1475, observed).
narrative_ontology:measurement(tech_su_t1490, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1490, 0.42).
narrative_ontology:measurement_basis(tech_su_t1490, observed).
narrative_ontology:measurement(tech_su_t1510, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1510, 0.52).
narrative_ontology:measurement_basis(tech_su_t1510, observed).
narrative_ontology:measurement(tech_su_t1530, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1530, 0.52).
narrative_ontology:measurement_basis(tech_su_t1530, observed).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1550, 0.52).
narrative_ontology:measurement_basis(tech_su_t1550, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1450, tn=1550
narrative_ontology:measurement(tech_grid_01, technology_reformation_causality__co_constitution_reading, accessibility_collapse(class), 1450, 0.05).
narrative_ontology:measurement(tech_grid_02, technology_reformation_causality__co_constitution_reading, accessibility_collapse(class), 1550, 0.55).
narrative_ontology:measurement(tech_grid_03, technology_reformation_causality__co_constitution_reading, accessibility_collapse(individual), 1450, 0.02).
narrative_ontology:measurement(tech_grid_04, technology_reformation_causality__co_constitution_reading, accessibility_collapse(individual), 1550, 0.38).
narrative_ontology:measurement(tech_grid_05, technology_reformation_causality__co_constitution_reading, accessibility_collapse(organizational), 1450, 0.08).
narrative_ontology:measurement(tech_grid_06, technology_reformation_causality__co_constitution_reading, accessibility_collapse(organizational), 1550, 0.68).
narrative_ontology:measurement(tech_grid_07, technology_reformation_causality__co_constitution_reading, accessibility_collapse(structural), 1450, 0.15).
narrative_ontology:measurement(tech_grid_08, technology_reformation_causality__co_constitution_reading, accessibility_collapse(structural), 1550, 0.72).
narrative_ontology:measurement(tech_grid_09, technology_reformation_causality__co_constitution_reading, resistance(class), 1450, 0.15).
narrative_ontology:measurement(tech_grid_10, technology_reformation_causality__co_constitution_reading, resistance(class), 1550, 0.52).
narrative_ontology:measurement(tech_grid_11, technology_reformation_causality__co_constitution_reading, resistance(individual), 1450, 0.12).
narrative_ontology:measurement(tech_grid_12, technology_reformation_causality__co_constitution_reading, resistance(individual), 1550, 0.38).
narrative_ontology:measurement(tech_grid_13, technology_reformation_causality__co_constitution_reading, resistance(organizational), 1450, 0.3).
narrative_ontology:measurement(tech_grid_14, technology_reformation_causality__co_constitution_reading, resistance(organizational), 1550, 0.72).
narrative_ontology:measurement(tech_grid_15, technology_reformation_causality__co_constitution_reading, resistance(structural), 1450, 0.25).
narrative_ontology:measurement(tech_grid_16, technology_reformation_causality__co_constitution_reading, resistance(structural), 1550, 0.68).
narrative_ontology:measurement(tech_grid_17, technology_reformation_causality__co_constitution_reading, stakes_inflation(class), 1450, 0.08).
narrative_ontology:measurement(tech_grid_18, technology_reformation_causality__co_constitution_reading, stakes_inflation(class), 1550, 0.48).
narrative_ontology:measurement(tech_grid_19, technology_reformation_causality__co_constitution_reading, stakes_inflation(individual), 1450, 0.05).
narrative_ontology:measurement(tech_grid_20, technology_reformation_causality__co_constitution_reading, stakes_inflation(individual), 1550, 0.32).
narrative_ontology:measurement(tech_grid_21, technology_reformation_causality__co_constitution_reading, stakes_inflation(organizational), 1450, 0.1).
narrative_ontology:measurement(tech_grid_22, technology_reformation_causality__co_constitution_reading, stakes_inflation(organizational), 1550, 0.62).
narrative_ontology:measurement(tech_grid_23, technology_reformation_causality__co_constitution_reading, stakes_inflation(structural), 1450, 0.12).
narrative_ontology:measurement(tech_grid_24, technology_reformation_causality__co_constitution_reading, stakes_inflation(structural), 1550, 0.68).
narrative_ontology:measurement(tech_grid_25, technology_reformation_causality__co_constitution_reading, suppression(class), 1450, 0.15).
narrative_ontology:measurement(tech_grid_26, technology_reformation_causality__co_constitution_reading, suppression(class), 1550, 0.48).
narrative_ontology:measurement(tech_grid_27, technology_reformation_causality__co_constitution_reading, suppression(individual), 1450, 0.08).
narrative_ontology:measurement(tech_grid_28, technology_reformation_causality__co_constitution_reading, suppression(individual), 1550, 0.28).
narrative_ontology:measurement(tech_grid_29, technology_reformation_causality__co_constitution_reading, suppression(organizational), 1450, 0.22).
narrative_ontology:measurement(tech_grid_30, technology_reformation_causality__co_constitution_reading, suppression(organizational), 1550, 0.64).
narrative_ontology:measurement(tech_grid_31, technology_reformation_causality__co_constitution_reading, suppression(structural), 1450, 0.18).
narrative_ontology:measurement(tech_grid_32, technology_reformation_causality__co_constitution_reading, suppression(structural), 1550, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__co_constitution_reading, 0.18).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel technology_reformation_causality. Sibling readings instantiate the same historical period and actors but attribute causality differently: technological_determinism_reading treats the press as the autonomous cause, beneficiary_agency_reading treats reformer agency as deterministic. Co-constitution_reading models bidirectional causality as the constraint's core claim. All three readings share the same interval (1450–1550) and many of the same stakeholders but model them with different directionality and extraction profiles. The ε values differ across readings because each reading instantiates a different causal structure applied to the same historical arrangement. They are linked as constraint family members via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, moderate, 0.38).
constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
