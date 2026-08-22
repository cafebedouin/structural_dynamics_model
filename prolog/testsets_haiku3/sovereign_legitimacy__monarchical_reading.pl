% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy: Divine Right and Hereditary Succession
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the MONARCHICAL READING of the contested
 *   kernel 'sovereign legitimacy': the claim that legitimate authority flows
 *   downward from a sovereign whose right to rule is grounded in divine
 *   sanction, ancestral bloodline, and traditional succession law. This is
 *   ONE of three structurally distinct readings of the same kernel (the other
 *   readings are the republican and constitutional-hybrid constraints,
 *   authored separately). The monarchical reading benefits the hereditary
 *   ruling class and aristocratic hierarchy while extracting obedience,
 *   labor, and taxation from excluded subjects. Its persistence depends on
 *   active suppression of alternative legitimacy claims (characterizing them
 *   as sedition or heresy) and on ritualized continuity (coronations,
 *   heraldic succession, religious validation). The foundational problem it
 *   claims to solve is succession crisis; the claim that bloodline rule
 *   prevents civil war is contested — democracies and republics have achieved
 *   peaceful succession through institutions unbounded by bloodline.
 *
 * KEY AGENTS:
 *   - hereditary_ruling_class: benefits, agenda-sets, identity-locked in noble status
 *   - aristocratic_hierarchy: benefits through inherited property and title, identity-locked in estate system
 *   - clergy_and_institutional_authorities: corroborate the divine mandate, half-beneficiary/half-enforcer
 *   - excluded_subjects: trapped commoners and peasants, bear taxation and labor extraction
 *   - non_noble_commoners: pay through taxation and subordination, constrained exit
 *   - alternative_legitimacy_claimants: structurally excluded republicans and democratic advocates
 *   - succession_rivals: caught between beneficiary status and payer status when the line is contested
 *   - international_rival_monarchies: maintain the reading globally through mutual recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.78).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.81).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy: Divine Right and Hereditary Succession").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, '2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa').
narrative_ontology:cs_kernel_codification('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', fixed_text).
narrative_ontology:cs_authority_grounding('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', lineage).
narrative_ontology:cs_interpretation_layer_present('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa').
narrative_ontology:cs_reading_relation('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', foundational, authority_descends_through_bloodline).
narrative_ontology:cs_axiom_status(authority_descends_through_bloodline, holdable).
narrative_ontology:cs_axiom_grounding('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', authority_descends_through_bloodline, conventional).
narrative_ontology:cs_axiom('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', foundational, divine_sanction_legitimates_monarchy).
narrative_ontology:cs_axiom_status(divine_sanction_legitimates_monarchy, holdable).
narrative_ontology:cs_axiom_grounding('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', divine_sanction_legitimates_monarchy, theological).
narrative_ontology:cs_axiom('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', secondary, traditional_succession_law_is_natural_order).
narrative_ontology:cs_axiom_status(traditional_succession_law_is_natural_order, overridden).
narrative_ontology:cs_axiom_grounding('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', traditional_succession_law_is_natural_order, deontological).
narrative_ontology:cs_reference_frame('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', established_hereditary_monarchy).
narrative_ontology:cs_drift_state('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', enlightenment_and_democratic_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2dfdd519-9647-4dfb-8bcd-2e4a6b6a2afa', '2026-08-03T14:22:18Z').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, excluded_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, non_noble_commoners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, clergy_and_institutional_authorities).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, non_noble_commoners).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, succession_claimants_and_rivals).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, divine_sanction_of_monarchy).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, bloodline_continuity_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, natural_order_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The monarch and hereditary nobility set the legitimacy framework and enforce it through law, ritual, and succession rules. Their authority derives from ancestral right and divine mandate. They administer the kingdom, collect rents (taxes, feudal dues, monopolies), and maintain the hierarchical order through appointment of officials and control of succession. Exit for this group means renouncing nobility and ancestral claims — a loss of identity and social position so complete that it functions as identity-lock despite theoretical availability.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, agenda_setter,
    institutional, generational, identity_locked, national).

% Lower nobility, gentry, and landed aristocrats benefit from the legitimacy framework by inheriting property, status, and governance positions. Their wealth and authority flow through the same hereditary-right mechanism that legitimates the crown. They participate in enforcement through local administration and are bound to the system by generations of familial investment in titles and estates.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, identity_locked, national).

% Religious institutions (especially established churches) and scholarly authorities corroborate and legitimize the monarch's divine mandate. They perform coronation rituals, preach obedience doctrine, and maintain theological justifications for monarchy. In return they receive endowments, legal privileges, and institutional security. They are semi-independent agenda-setters in that they interpret and authenticate the legitimacy doctrine, but constrained by the monarch's power to suppress heterodox readings.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, clergy_and_institutional_authorities, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, clergy_and_institutional_authorities, agenda_setter).

% Commoners, peasants, merchants, and non-noble persons have no formal role in setting legitimacy. They owe obedience, taxes, and service. They are trapped by law (no legal right to emigrate or refuse authority), by economic dependency (subsistence on land they do not own), and by military enforcement. The legitimacy framework explicitly denies them voice in governance and constrains their exit options to flight (leaving the kingdom) or rebellion (which the framework treats as illegitimate). Their suppression is structural: the very claim that legitimacy flows downward by bloodline erases their capacity to challenge it.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, excluded_subjects, payer,
    powerless, biographical, trapped, national).

% Merchants, craftspeople, and a rising professional class pay through taxation and subordination to noble jurisdiction. They may benefit incidentally from stable legal order and trade protection that the monarchy provides. Their exit is constrained: they have some geographic mobility and property rights (unlike peasants) but cannot leave the kingdom without abandoning livelihood and community. They are positioned between peasants and nobility — better-positioned to perceive the arbitrariness of hereditary authority but structurally unable to organize against it.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, non_noble_commoners, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, non_noble_commoners, beneficiary).

% Persons and movements advocating for alternative legitimacy bases (republican government, popular sovereignty, merit-based authority, religious heterodoxy) are structurally excluded from institutional voice. They would argue that legitimacy flows upward from the people, not downward from blood and divinity. Their exclusion is maintained by treating alternative claims as sedition, heresy, or treason — categories the monarchical framework itself defines. Trapped because challenging the framework within its territory means confrontation with enforcement apparatus.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, excluded_alternative_legitimacy_claimants, excluded,
    powerful, generational, trapped, national).

% Cadet branches of the dynasty, collateral relatives, and claimants to the throne are caught between beneficiary status (noble blood) and payer status (excluded from actual authority until succession opens). During contested successions they may pay through internal conflict, civil war, or assassination risk. Their constraint is bitter: the very mechanism that legitimates one heir delegitimizes all others, yet bloodline gives them plausible claims. They cannot fully exit because their status is derived from the dynasty itself.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, succession_claimants_and_rivals, excluded,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, succession_claimants_and_rivals, payer).

% Other monarchies and rival powers are outside the legitimacy framework but constrained by it through the principle of mutual recognition: monarchs recognize each other's divine right (establishing a peer-to-peer network of monarchical legitimacy). This serves their collective interest in suppressing republican and popular-sovereignty movements that might spread across borders. They maintain the monarchical reading internationally while remaining outside any single kingdom's enforcement apparatus.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, international_rivals_and_foreign_powers, excluded,
    institutional, generational, mobile, global).

% The institutional machinery of records, genealogy, ritual, and symbolic continuity that authenticates bloodline legitimacy. This is not an actor but a distributed system the constraint depends on — records of lineage, coronation protocols, heraldic standards, succession law. Analytical vantage for observing how legitimacy claims depend on continuous narration of unbroken descent.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, historical_continuity_apparatus, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sovereign_legitimacy__monarchical_reading, historical_continuity_apparatus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, determinate succession rule: the crown passes by blood to a legally defined heir. This solves the coordination problem of how to transfer supreme authority without civil war at each death — all parties know in advance (by the rule of primogeniture or other fixed succession law) who the next sovereign will be. It also coordinates the subordinate nobility around a stable hierarchy: property, titles, and offices flow through the same bloodline mechanism, creating a predictable system of inheritance and station.
% TRANSFER_FUNCTION: Moves wealth upward to the crown (through taxation, feudal dues, monopolies, and confiscation) and rightward through generations of the dynasty (through hereditary property and title). It also transfers obedience and labor services downward from subjects to rulers. At the ideological level it transfers legitimacy-validation authority to clergy and court scholars who authenticate the divine mandate and bloodline doctrine.
% ABSENT_VOICES: Excluded subjects and alternative legitimacy movements. Peasants and commoners, who bear the heaviest extraction cost through taxation and labor obligation, are structurally barred from any institutional voice in the system. Advocates for popular sovereignty, republican government, and merit-based authority are treated as seditionists and heretics — their exclusion is maintained by the framework itself, which defines legitimacy downward and delegitimizes upward claims as illegitimate by definition. No democratic institutions, no representative assemblies of the commons, no challenge to bloodline doctrine are permitted architectural presence.
% DISAPPEARANCE_RATIONALE: If the monarchical legitimacy framework and its enforcement disappeared overnight, succession would become contestable, the hereditary claim to property and title would lose legal backing, taxation and feudal dues would cease, and the entire hierarchy of station would collapse. The ruling class would lose both revenue and legitimacy. Subjects would face immediate power vacuums and conflict, but the removal of the framework's suppression of alternative legitimacy claims would enable reorganization around different principles (republican, democratic, meritocratic). The world would rearrange substantially — property systems, authority structures, and social hierarchy would all be in play.
% FOUNDING_PROBLEM: The founding problem is the succession crisis: how to transfer supreme authority upon a sovereign's death without civil war, assassination, and the collapse of all subordinate authority. The monarchical reading solves it through blood and law: by declaring that succession is determined by descent, not by the will of the dead sovereign, not by conquest, and not by election.
% FOUNDING_PROBLEM_CORROBORATION: Monarchical courts, clergy, and nobility attest the founding problem remains live — succession disputes and the threat of civil war at every generational transfer. Republican and democratic critics attest the founding problem is solved by alternative means: written constitutions, electoral procedures, institutional continuity that do not depend on bloodline. Historical analysis from outside the benefiting parties (democratic-theory scholars, historians of republicanism, constitutional lawyers) shows succession disputes persist even within strict bloodline rules (collateral claims, bastard claims, female-succession disputes) and that many successions have been peaceful without monarchical rules — the founding problem is better characterized as 'managed succession without bloodline rule' rather than 'solved by monarchy.'
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness runs at 0.78 because the monarchical framework extracts wealth upward (taxation, feudal dues, monopolies) and constrains exit downward (law forbids emigration, enforces obedience). It is not pure extraction because the constraint does provide coordination benefits: stable succession, clear hierarchy, unified authority (eliminating the problem of competing centers). This is why it is claimed as tangled_rope (genuine coordination + asymmetric extraction) rather than snare. Suppression is high (0.81) because the framework's persistence depends on actively suppressing alternative legitimacy claims — any claim that authority flows upward from the people is treated as heresy or sedition, and the framework itself defines this suppression as legitimate use of authority. Accessibility collapse is very high (0.88) because once one accepts the monarchical reading, alternatives (republicanism, popular sovereignty, merit-based authority) collapse as incoherent — they require rejecting the foundational premise that blood carries authority. Theater ratio rises modestly from 0.48 to 0.62 over the interval: this tracks the increasing cost of maintaining the theatrical apparatus of legitimacy (coronations, processions, genealogical records, religious validation) as the material basis of monarchy weakens under pressure from printing, mercantilism, and emerging democratic movements. The theater becomes more visible precisely when the functional coordination benefits are declining and the extraction benefits become the primary object of enforcement. Resistance also rises (0.48→0.54 at structural level, 0.45→0.48 at individual level) but stays well below suppression, indicating the framework contains dissent through legal prohibition and ideological dominance rather than defeating it in open competition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (the ruling class) and the payer seats (excluded subjects, commoners) compute dramatically different types from identical structural data. From the crown's position, the arrangement is genuine coordination: the succession rule IS the price of stability and unified authority, and the subjects' obedience is reciprocated by governance and protection (the contract of monarchy in early modern theory). From the subject's position, the same structure is extraction clothed in legitimacy theater: their obedience is coerced, their exit is blocked by law and economic dependency, and the 'stability' they receive is the absence of worse instability — they pay whether the coordination function is real or not. The engine computes this divergence directly from the structural directionality of each seat (beneficiary → low d; payer → high d; identity-locked → amplified d), without requiring any classifier to resolve whose frame is correct. The claim is tangled_rope; the computed type at the beneficiary seat may lean rope; the computed type at the payer seat will lean snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Hereditary ruling class: d near 0.0 (beneficiary end) because they collect extraction rents (taxation, feudal dues, monopolies, land monopoly), they define the rules, and their exit options are identity-locked (renouncing nobility would mean losing everything they are — their identity is fused with their position). Aristocratic hierarchy: d similarly low (0.05–0.15 range), beneficiary through inherited property and status, identity-locked through generations of familial investment in estates. Clergy: d near 0.15–0.25, dual-positioned as both beneficiary (endowments, legal privilege) and partial agenda-setter (they authenticate and interpret), constrained exit because challenging the divine-mandate doctrine would forfeit their institutional security. Excluded subjects: d near 1.0 (target end) because they bear extraction costs (taxation, labor obligation), have trapped exit (law forbids emigration, economic dependency prevents flight), and are denied voice in rule-setting. Non-noble commoners: d near 0.75–0.85 (closer to target than middle) because they pay taxes and owe subordination, though they have slightly more mobility and property rights than peasants; identity is not fused into the framework the way hereditary nobility's is. No directionality overrides are needed — the structural derivation (beneficiary/victim + exit + power) produces accurate d values for all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy status: CONTESTED. The founding problem the monarchical reading claims to solve is succession crisis — the need for a determinate rule to prevent civil war when authority transfers. By historical evidence this problem is SOLVED: stable succession can be achieved through written constitutions, electoral procedures, and institutional continuity that do not depend on bloodline. Modern democracies have solved succession without monarchy. However, the beneficiaries (ruling class) have not abandoned the constraint because bloodline rule now serves a different function: property inheritance, status preservation, and the coordination of elite hierarchy. The original coordination justification has atrophied, but the extraction apparatus persists. This is the piton signature: high theater ratio (rising from 0.48 to 0.62), persistence without substantial coordination payoff to most stakeholders, and functional inertia. BUT: the claim is tangled_rope, which asserts that genuine coordination remains. This apparent divergence is resolved by recognizing SEAT DIVERGENCE: from the beneficiary's position (crown, nobility), the constraint still provides coordination (orderly succession, hierarchy definition, institutional continuity). From the payer's position (commoners, subjects), the coordination function is gone and only extraction remains. The tangled-rope claim is coherent at the agenda-setter's seat but would compute as piton or snare at the payer's seat. The mandatrophy container is the mismatch between founding problem status (dead or contested) and disappearance verdict (world_rearranges only in elite power distribution; subjects would reorganize around different authority principles whether monarchy persists or not). The reading handles this through the landing omega on persistence mechanisms: as the ideological appeal to bloodline weakens, what holds the constraint in place? Answer: identity-lock of the beneficiaries, relative weakness of organized resistance, and international mutual recognition among rival monarchies (each defends the others to suppress republicanism globally).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_empirical_content,
    'Is the divine sanction grounding of monarchical legitimacy an empirically testable claim about the metaphysical order, or a conventional social assertion with no empirical content?',
    'This is a conceptual boundary question: does the reading claim that God actively sanctions monarchy (testable in principle, though never demonstrably refuted), or merely that invoking divine will is a legitimate political move (testable sociologically — does the population accept it as legitimate)? The first is theology; the second is narrative power.',
    'If testable-in-principle (the first), then competing readings that deny divine sanction directly contradict this reading, making the relation ''forecloses'' rather than ''coexists_with''. If conventional (the second), readings can coexist because they are competing narratives, not contradictory metaphysical claims. The classification at the structural level does not change, but the logical structure of the kernel does.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_sanction_empirical_content, conceptual, 'Whether the divine mandate claim is metaphysically or conventionally grounded.').

omega_variable(
    identity_lock_mechanism_in_ruling_class,
    'Is the hereditary ruling class bound to the monarchical constraint primarily by identity-fusion (noble self-concept cannot be separated from noble position), by property interest (immense wealth in land and titles whose value depends on hereditary inheritance), or by both equally?',
    'Comparative analysis of nobility in declining monarchies: if the ruling class exits the constraint when property protections are offered (as in constitutional monarchy transitions), the mechanism is primarily interest-based. If they resist property protection and cling to title and bloodline narrative (as in some aristocratic-honor cultures), identity is the primary mechanism. Most cases show both, but the ratio varies by culture and historical moment.',
    'If identity-fusion is the dominant mechanism, the constraint persists even when material interest would suggest exit (e.g., a noble family keeps title despite loss of extraction revenue). If property interest dominates, the constraint becomes vulnerable to reforms that protect property through alternative mechanisms (constitutionalism, meritocracy). Directionality of the beneficiary seat would shift slightly if the exit mechanism shifted from identity-locked to constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_ruling_class, empirical, 'What binds the beneficiary class to the monarchical rule — identity or interest.').

omega_variable(
    suppression_internalization_in_subjects,
    'The high suppression score (0.81) reflects structural barriers (law, military enforcement, economic dependency) to challenging monarchy. Is suppression also substantially internalized — do subjects believe they ought to obey, or do they obey only because they must?',
    'Post-authority-collapse evidence: if suppression persists after the legal and military enforcement apparatus is removed, internalization was substantial. If subjects rapidly reorganize around alternative authority once constraints are lifted, suppression was primarily structural. Historical data from revolutionary transitions shows mixed results — some populations internalize obedience for generations even after authority collapses; others enthusiastically adopt alternatives.',
    'Internalized suppression is more resilient to enforcement decay and political crisis. It makes the constraint more piton-like (persisting through theater and habit even when enforcement is expensive). Structural suppression is more vulnerable to organized resistance and elite defection. The measured suppression score (0.81) represents the total (structural + internalized); the ratio is what this omega attempts to locate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_subjects, empirical, 'The internalized vs. structural composition of subject-level suppression.').

omega_variable(
    coordination_function_persistence,
    'The claimed coordination function is determinate succession. But does the monarchical mechanism actually provide this, or do succession disputes (collateral claims, bastard claims, gender succession disputes, regency disputes) remain endemic to blood-based rules?',
    'Comparative frequency analysis: count the succession disputes in a sample of monarchies across centuries versus the succession disputes in a sample of constitutional democracies over equivalent time periods. If bloodline rules prevent fewer disputes, the coordination function is real. If the frequency is similar or bloodline rules generate their own dispute mechanisms, the function is not doing the work the reading claims.',
    'If succession disputes remain frequent under blood rule, then the founding problem is NOT solved, and the constraint becomes less tangled_rope (coordination function attenuated) and more snare-like (extraction without genuine payoff). This would support mandatrophy analysis — the founding justification is hollow and only extraction remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_persistence, empirical, 'Whether bloodline succession actually prevents the succession disputes it claims to prevent.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do the monarchical and republican readings FORECLOSE each other (logically rule each other out in a single framework), or do they merely COEXIST as competing parties'' positions in an ongoing dispute?',
    'Strict logical analysis of the foundational axioms: if the monarchical reading''s core claim (''authority legitimately flows downward from blood/divinity'') is logically contradicted by the republican reading''s core claim (''authority legitimately flows upward from the people''), then they foreclose. If both can be held in a single framework (e.g., ''the people consented once to a hereditary rule; that consent now binds''), they coexist. Constitutional-hybrid readings that hold both simultaneously are evidence of coexistence.',
    'If readings foreclose, the kernel is a zero-sum contest and one reading''s survival requires the other''s defeat. If they coexist, the kernel accommodates multiple parties and readings persist together through political cohabitation or institutional compartmentalization. The relation between siblings changes from ''forecloses'' to ''coexists_with'', affecting how contamination/closure dynamics model the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether sibling readings are logically contradictory or merely competing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(sove_tr_t0, observed).
narrative_ontology:measurement(sove_tr_t5, sovereign_legitimacy__monarchical_reading, theater_ratio, 5, 0.51).
narrative_ontology:measurement_basis(sove_tr_t5, observed).
narrative_ontology:measurement(sove_tr_t10, sovereign_legitimacy__monarchical_reading, theater_ratio, 10, 0.54).
narrative_ontology:measurement_basis(sove_tr_t10, observed).
narrative_ontology:measurement(sove_tr_t15, sovereign_legitimacy__monarchical_reading, theater_ratio, 15, 0.57).
narrative_ontology:measurement_basis(sove_tr_t15, observed).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__monarchical_reading, theater_ratio, 20, 0.59).
narrative_ontology:measurement_basis(sove_tr_t20, observed).
narrative_ontology:measurement(sove_tr_t25, sovereign_legitimacy__monarchical_reading, theater_ratio, 25, 0.61).
narrative_ontology:measurement_basis(sove_tr_t25, observed).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__monarchical_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement_basis(sove_tr_t30, observed).
narrative_ontology:measurement(sove_tr_t35, sovereign_legitimacy__monarchical_reading, theater_ratio, 35, 0.62).
narrative_ontology:measurement_basis(sove_tr_t35, observed).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__monarchical_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(sove_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(sove_be_t0, observed).
narrative_ontology:measurement(sove_be_t5, sovereign_legitimacy__monarchical_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement_basis(sove_be_t5, observed).
narrative_ontology:measurement(sove_be_t10, sovereign_legitimacy__monarchical_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(sove_be_t10, observed).
narrative_ontology:measurement(sove_be_t15, sovereign_legitimacy__monarchical_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement_basis(sove_be_t15, observed).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__monarchical_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(sove_be_t20, observed).
narrative_ontology:measurement(sove_be_t25, sovereign_legitimacy__monarchical_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(sove_be_t25, observed).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__monarchical_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(sove_be_t30, observed).
narrative_ontology:measurement(sove_be_t35, sovereign_legitimacy__monarchical_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(sove_be_t35, observed).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__monarchical_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(sove_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement_basis(sove_su_t0, observed).
narrative_ontology:measurement(sove_su_t5, sovereign_legitimacy__monarchical_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement_basis(sove_su_t5, observed).
narrative_ontology:measurement(sove_su_t10, sovereign_legitimacy__monarchical_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement_basis(sove_su_t10, observed).
narrative_ontology:measurement(sove_su_t15, sovereign_legitimacy__monarchical_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement_basis(sove_su_t15, observed).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__monarchical_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement_basis(sove_su_t20, observed).
narrative_ontology:measurement(sove_su_t25, sovereign_legitimacy__monarchical_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement_basis(sove_su_t25, observed).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__monarchical_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement_basis(sove_su_t30, observed).
narrative_ontology:measurement(sove_su_t35, sovereign_legitimacy__monarchical_reading, suppression_requirement, 35, 0.81).
narrative_ontology:measurement_basis(sove_su_t35, observed).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__monarchical_reading, suppression_requirement, 40, 0.81).
narrative_ontology:measurement_basis(sove_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(sove_grid_01, sovereign_legitimacy__monarchical_reading, accessibility_collapse(class), 0, 0.78).
narrative_ontology:measurement(sove_grid_02, sovereign_legitimacy__monarchical_reading, accessibility_collapse(class), 40, 0.8).
narrative_ontology:measurement(sove_grid_03, sovereign_legitimacy__monarchical_reading, accessibility_collapse(individual), 0, 0.65).
narrative_ontology:measurement(sove_grid_04, sovereign_legitimacy__monarchical_reading, accessibility_collapse(individual), 40, 0.68).
narrative_ontology:measurement(sove_grid_05, sovereign_legitimacy__monarchical_reading, accessibility_collapse(organizational), 0, 0.82).
narrative_ontology:measurement(sove_grid_06, sovereign_legitimacy__monarchical_reading, accessibility_collapse(organizational), 40, 0.85).
narrative_ontology:measurement(sove_grid_07, sovereign_legitimacy__monarchical_reading, accessibility_collapse(structural), 0, 0.85).
narrative_ontology:measurement(sove_grid_08, sovereign_legitimacy__monarchical_reading, accessibility_collapse(structural), 40, 0.88).
narrative_ontology:measurement(sove_grid_09, sovereign_legitimacy__monarchical_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(sove_grid_10, sovereign_legitimacy__monarchical_reading, resistance(class), 40, 0.62).
narrative_ontology:measurement(sove_grid_11, sovereign_legitimacy__monarchical_reading, resistance(individual), 0, 0.45).
narrative_ontology:measurement(sove_grid_12, sovereign_legitimacy__monarchical_reading, resistance(individual), 40, 0.48).
narrative_ontology:measurement(sove_grid_13, sovereign_legitimacy__monarchical_reading, resistance(organizational), 0, 0.52).
narrative_ontology:measurement(sove_grid_14, sovereign_legitimacy__monarchical_reading, resistance(organizational), 40, 0.56).
narrative_ontology:measurement(sove_grid_15, sovereign_legitimacy__monarchical_reading, resistance(structural), 0, 0.48).
narrative_ontology:measurement(sove_grid_16, sovereign_legitimacy__monarchical_reading, resistance(structural), 40, 0.52).
narrative_ontology:measurement(sove_grid_17, sovereign_legitimacy__monarchical_reading, stakes_inflation(class), 0, 0.68).
narrative_ontology:measurement(sove_grid_18, sovereign_legitimacy__monarchical_reading, stakes_inflation(class), 40, 0.7).
narrative_ontology:measurement(sove_grid_19, sovereign_legitimacy__monarchical_reading, stakes_inflation(individual), 0, 0.62).
narrative_ontology:measurement(sove_grid_20, sovereign_legitimacy__monarchical_reading, stakes_inflation(individual), 40, 0.64).
narrative_ontology:measurement(sove_grid_21, sovereign_legitimacy__monarchical_reading, stakes_inflation(organizational), 0, 0.72).
narrative_ontology:measurement(sove_grid_22, sovereign_legitimacy__monarchical_reading, stakes_inflation(organizational), 40, 0.75).
narrative_ontology:measurement(sove_grid_23, sovereign_legitimacy__monarchical_reading, stakes_inflation(structural), 0, 0.79).
narrative_ontology:measurement(sove_grid_24, sovereign_legitimacy__monarchical_reading, stakes_inflation(structural), 40, 0.81).
narrative_ontology:measurement(sove_grid_25, sovereign_legitimacy__monarchical_reading, suppression(class), 0, 0.74).
narrative_ontology:measurement(sove_grid_26, sovereign_legitimacy__monarchical_reading, suppression(class), 40, 0.76).
narrative_ontology:measurement(sove_grid_27, sovereign_legitimacy__monarchical_reading, suppression(individual), 0, 0.66).
narrative_ontology:measurement(sove_grid_28, sovereign_legitimacy__monarchical_reading, suppression(individual), 40, 0.68).
narrative_ontology:measurement(sove_grid_29, sovereign_legitimacy__monarchical_reading, suppression(organizational), 0, 0.78).
narrative_ontology:measurement(sove_grid_30, sovereign_legitimacy__monarchical_reading, suppression(organizational), 40, 0.8).
narrative_ontology:measurement(sove_grid_31, sovereign_legitimacy__monarchical_reading, suppression(structural), 0, 0.81).
narrative_ontology:measurement(sove_grid_32, sovereign_legitimacy__monarchical_reading, suppression(structural), 40, 0.83).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__monarchical_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% The constraint 'sovereign_legitimacy' is a kernel with three structurally distinct readings: monarchical (this file), republican, and constitutional-hybrid. Each reading instantiates a different constraint with its own ε, beneficiary/victim structure, suppression mechanism, and classification. They are linked here as siblings in the same kernel family. The kernel is the commitment to determinate succession rules; the readings differ in which mechanism determines succession (bloodline, popular consent, constitutional delegation). See commentary.kernel_context for the differentiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
