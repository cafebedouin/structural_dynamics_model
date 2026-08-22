% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy via Divine Right and Hereditary Succession
 *   domain: political_philosophy/constitutional_theory/legitimacy
 *
 * SUMMARY:
 *   The monarchical reading instantiates one framing of the contested kernel
 *   'sovereign legitimacy': legitimate political authority flows downward
 *   from the sovereign, grounded in hereditary right, divine sanction,
 *   tradition, and bloodline continuity. This is one reading among three: the
 *   republican reading (authority flows upward from popular sovereignty and
 *   consent) and the constitutional-hybrid reading (authority is
 *   dual-sourced: ceremonial/symbolic inherited, political delegated, with
 *   law mediating) are structurally incompatible readings held by different
 *   parties in ongoing dispute. The monarchical reading frames the hereditary
 *   ruling class as beneficiary, disenfranchised subjects as victims, and
 *   high suppression as necessary to prevent rival legitimacy claims from
 *   surfacing. The claim is Tangled Rope: genuine coordination function
 *   (unified rule, predictable succession) coupled with asymmetric extraction
 *   (authority and revenue to the ruling class, exclusion and obligation from
 *   subjects). The metrics describe operation over four decades:
 *   extractiveness rises gradually as the constraint matures, theater ratio
 *   plateaus as the ritual machinery stabilizes, and suppression remains high
 *   and constant, indicating the constraint depends on continuous enforcement
 *   against excluded voices.
 *
 * KEY AGENTS:
 *   - hereditary_ruling_class: agenda-setter and primary beneficiary; captures political authority and state revenue; vulnerable to succession contests
 *   - aristocratic_hierarchy: secondary beneficiary; enforces the constraint through administrative machinery; bound by hereditary status
 *   - disenfranchised_subjects: primary victims; bear costs of taxation, conscription, legal subordination; trapped by geographic and coordination barriers
 *   - religious_institutional_authority: beneficiary of validation role; locked in partnership with crown; provides divine sanction
 *   - rival_claimants: excluded from legitimacy contest; would benefit from open succession rules; kept out by enforcement
 *   - reform_philosophers: excluded voices; would articulate alternative legitimacy claims; suppressed by censorship/exile/execution
 *   - analytical observer: examines the constraint structure across time and rival readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.78).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.82).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy via Divine Right and Hereditary Succession").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory/legitimacy").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, '22f08134-d9e5-4b0b-aa31-351bfa318c8e').
narrative_ontology:cs_kernel_codification('22f08134-d9e5-4b0b-aa31-351bfa318c8e', fixed_text).
narrative_ontology:cs_authority_grounding('22f08134-d9e5-4b0b-aa31-351bfa318c8e', lineage).
narrative_ontology:cs_interpretation_layer_present('22f08134-d9e5-4b0b-aa31-351bfa318c8e').
narrative_ontology:cs_reading_relation('22f08134-d9e5-4b0b-aa31-351bfa318c8e', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_reading_relation('22f08134-d9e5-4b0b-aa31-351bfa318c8e', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('22f08134-d9e5-4b0b-aa31-351bfa318c8e', foundational, hereditary_succession_legitimates_authority).
narrative_ontology:cs_axiom_status(hereditary_succession_legitimates_authority, holdable).
narrative_ontology:cs_axiom_grounding('22f08134-d9e5-4b0b-aa31-351bfa318c8e', hereditary_succession_legitimates_authority, conventional).
narrative_ontology:cs_axiom('22f08134-d9e5-4b0b-aa31-351bfa318c8e', foundational, divine_sanction_doctrine).
narrative_ontology:cs_axiom_status(divine_sanction_doctrine, overridden).
narrative_ontology:cs_axiom_grounding('22f08134-d9e5-4b0b-aa31-351bfa318c8e', divine_sanction_doctrine, theological).
narrative_ontology:cs_axiom('22f08134-d9e5-4b0b-aa31-351bfa318c8e', secondary, bloodline_continuity_natural_law).
narrative_ontology:cs_axiom_status(bloodline_continuity_natural_law, overridden).
narrative_ontology:cs_axiom_grounding('22f08134-d9e5-4b0b-aa31-351bfa318c8e', bloodline_continuity_natural_law, empirically_contingent).
narrative_ontology:cs_reference_frame('22f08134-d9e5-4b0b-aa31-351bfa318c8e', divinely_sanctioned_hereditary_monarchy).
narrative_ontology:cs_drift_state('22f08134-d9e5-4b0b-aa31-351bfa318c8e', modernity_and_secularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('22f08134-d9e5-4b0b-aa31-351bfa318c8e', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, disenfranchised_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, non_noble_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, non_noble_populations).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, religious_institutional_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reigning monarch and immediate family control the state apparatus, make binding law, and claim authority flows to them by divine sanction and bloodline right. They benefit from the constraint by capturing all major political authority and command loyalty without popular consent mechanisms. Their exit is escape from rule itself (abdication or deposition), available only at enormous cost to their status and patrimony.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, beneficiary).

% Noble houses preserve privilege, land holdings, and political voice through inherited rank and proximity to the throne. They benefit from exclusion of non-noble populations and enforce the constraint through administrative and legal machinery. Their exit is downward mobility and loss of noble status; they maintain the constraint because its collapse threatens their entire social position.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, agenda_setter).

% The bulk of the population has no formal voice in governance, no claim to authority, and no mechanism to withdraw consent. They bear the costs of the arrangement through taxation, conscription, labor obligation, and legal subordination. Their only exit is geographic (migration, emigration) or revolutionary (regime overthrow), both expensive and dangerous. The constraint's persistence depends on their inability to coordinate alternative legitimacy claims.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, disenfranchised_subjects, payer,
    powerless, biographical, trapped, national).

% Merchants, clergy, professionals, and landed gentry occupy intermediate rank. They are excluded from top political authority and subject to royal law, yet have some economic power and local influence. They pay through compliance, deference, and potential conscription; they are constrained by law from asserting legitimacy claims and by strategic disadvantage from mobilizing collectively against the crown. They occupy the ambiguous position: benefiting from order the crown provides while bearing the costs of subordination.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, non_noble_populations, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, non_noble_populations, beneficiary).

% The established church (or equivalent religious body) validates the monarch's divine sanction, performs coronation rituals, and teaches subjects that rebellion violates divine law. It collects legitimacy claims and material resources in exchange for this validation. It is bound to the crown but not subordinate; exit means losing the crown's protection and patronage, making it effectively trapped in the partnership.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, religious_institutional_authority, beneficiary,
    institutional, generational, constrained, national).

% Other noble families with genealogical claims to the throne are structurally barred from legitimacy. The reigning line's monopoly on succession is maintained by law and force; rival claimants remain excluded despite sometimes having plausible hereditary arguments. They would benefit from an open legitimacy contest but are kept out by the same inheritance rules the reigning line uses to stay in power.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, rival_claimants_and_pretenders, excluded,
    powerful, biographical, identity_locked, national).

% Intellectuals and political thinkers who articulate republican or constitutional alternatives are censored, exiled, or executed when their ideas gain traction. They are excluded from legitimate public discourse by the same suppression mechanism that enforces the constraint. Their voices would directly challenge the divine-right and bloodline axioms; suppression of these voices is the enforcement work.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, reform_philosophers_and_critics, excluded,
    moderate, biographical, trapped, national).

% Historians, political theorists, and contemporary analysts examine the constraint's operation across time and context. They observe the claim structure, the mechanisms of enforcement, the rituals that sustain it, and the contradictions it faces when succession becomes contested or foreign powers deny the legitimacy frame.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, clear decision-making center with predictable succession, solving the collective-action problem of coordinating many subjects under one rule without negotiating consent. The monarch provides order, security, and unified law-making; subjects know the rules, the ruler, and the succession line in advance.
% TRANSFER_FUNCTION: Moves political authority and the revenues of the state (taxes, labor, military conscription, feudal rents) from the disenfranchised population to the hereditary ruling class. The transfer is justified as the price of the coordination the monarchy provides; enforced by the claim that authority rightfully belongs to the bloodline by divine will and inheritance.
% ABSENT_VOICES: The disenfranchised subject population has no formal mechanism to voice grievance or dissent. Reform philosophers and rival claimants are actively suppressed. The excluded voices would object that the divine-right claim is unfounded, that authority should flow from popular consent or constitutional law, and that the hereditary monopoly is arbitrary extraction rather than natural order.
% DISAPPEARANCE_RATIONALE: If the monarchical legitimacy claim and its enforcement apparatus vanished overnight, political authority would reorganize: rival claimants would fight for succession, subjects would demand voice in governance, and a new legitimacy mechanism (republican, constitutional, or oligarchic) would emerge to fill the vacuum. The monarch depends on the constraint's persistence; its disappearance is the monarch's deposition.
% FOUNDING_PROBLEM: Primordial political question: how to coordinate many people under one ruler without constant consent-seeking and renegotiation. The monarchical answer: hereditary right, sanctioned by divine authority and enforced through ritual and law, solves succession predictability and removes legitimacy from popular contests.
% FOUNDING_PROBLEM_CORROBORATION: Monarchical defenders attest the founding problem remains live: rapid succession contests, popular chaos, and warring factions would emerge without clear hereditary rules; history provides examples. Disenfranchised subjects and republican critics attest the problem is a false framing: order and succession are solvable without divine sanction or hereditary monopoly, as demonstrated by constitutional and republican systems; the 'problem' is a cover story for extraction. Historians and political theorists document that the founding problem is genuinely contingent: it was solved differently in different contexts, and the monarchical solution was neither inevitable nor obviously superior to alternatives.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.78) is high because the constraint concentrates political authority and state revenues in the ruling class without requiring their consent or input from subjects. The transfer from subjects to rulers is justified by divine will and tradition rather than market price or negotiated rate, making it extraction rather than exchange. Suppression (0.82) is higher still because the constraint's persistence depends critically on preventing subjects and philosophers from articulating rival legitimacy claims. The enforcement machinery (censorship, religious sanction, legal subordination, removal of rival claimants) is not incidental but central to maintaining the divine-right frame. Theater ratio (0.61) indicates the constraint relies substantially on performative ritual—coronation ceremonies, royal procession, religious validation—to make the bloodline claim seem natural and inevitable. The measured trajectory shows extractiveness rising and plateauing as the constraint consolidates (initial period 0-15 shows growth as enforcement hardens; period 15-40 shows stabilization once enforcement reaches sustainable capacity). Suppression rises sharply early and plateaus, indicating the enforcement regime reaches its effective ceiling once rival claimants are eliminated and philosophical dissent is driven underground.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (hereditary ruling class), the constraint is a natural solution to a genuine coordination problem: how to govern without constant renegotiation. The divine-right frame is not seen as extraction but as the legitimate source of authority. From the victim seat (disenfranchised subjects), the same constraint is pure extraction with a cover story: divine sanction and bloodline continuity are claims unsupported by evidence, used to justify the confiscation of political voice and resources. From the intermediate seat (non-noble populations), the constraint provides order and security (beneficiary function) while excluding them from top authority (victim function); they hold dual roles because the constraint's classification diverges radically depending on whether one emphasizes the coordination benefit or the exclusionary cost. The engine computes these seat divergences from the structural data: beneficiary + high power + arbitrage exit → low d (beneficiary computation); victim + powerless + trapped exit → high d (target computation). The authored claim (Tangled Rope) reflects the fact that both the coordination function and the extraction asymmetry are structurally real, not that the disagreement between seats is an illusion.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary ruling class and aristocracy sit at the beneficiary end of directionality (d near 0.0): they collect authority and revenue without bearing the costs of popular accountability or legitimacy contests. Their exit is escape from rule entirely (abdication), which costs them everything, making them trapped within the constraint despite being its primary beneficiaries. Disenfranchised subjects sit at the target end (d near 1.0): they bear the costs of taxation, conscription, legal subordination, and exclusion without receiving political voice or authority. Their exit options are geographic (migration, near-impossible in premodern contexts) or revolutionary (collective overthrow, requires coordination against suppression). Non-noble intermediate populations sit near symmetric (d around 0.5 to 0.6): they benefit from order and security but bear the costs of subordination and legal exclusion. The religious institutional authority is a secondary beneficiary (d near 0.2): it collects legitimacy claims and material resources in exchange for validation, but is constrained by its partnership with the crown and cannot exit without losing both protection and patronage. Rival claimants and reform philosophers are excluded rather than coordinate or paid, making directionality analysis complex: they have high potential d (they would be targets if admitted) but are structurally barred from the game entirely by enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint faces a mandatrophy risk (founding problem outlived): the foundational problem was succession stability in premodern contexts where bloodline and divine sanction provided the clearest answer. As societies modernize, alternatives become visible and attractive: constitutional law provides succession rules without divine claims; electoral systems provide legitimacy without heredity; bureaucratic institutions provide order without monarchy. The constraint persists (as measured by non-zero extractiveness and suppression across the entire interval) through theatrical maintenance and active enforcement, not because the founding problem remains live. The theater ratio (0.61) indicates nearly two-thirds of enforcement energy goes to performance—coronations, religious ritual, pageantry, legal ceremony—rather than to solving the actual coordination problem, which could be solved equally well by constitutional succession rules. The suppression requirement (0.82) indicates the constraint depends critically on preventing subjects and philosophers from discovering the alternatives. A mandatrophy resolution would require either: (a) demonstrating the founding problem is still live (i.e., modern constitutional systems do NOT provide stable succession without heredity), which is factually false in most contemporary examples, or (b) reclassifying the constraint from Tangled Rope (where the extraction is asymmetric but the coordination is real) to Snare (where the coordination claim is mere cover). The measurement trajectory suggests approach to Snare as modernity advances: extractiveness plateaus but theater_ratio does not decrease despite the founding problem becoming less visible, indicating theatrical maintenance increasingly dominant over functional need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_grounding,
    'Is divine sanction for hereditary authority a genuine source of legitimacy, or a constructed narrative that the ruling class uses to justify extraction?',
    'No empirical resolution exists; this is a foundational axiom of the monarchical reading. Different parties hold different answers: the reading assumes divine sanction is real; critics deny it. Resolution would require theological consensus or demonstrated correlation between claimed divine favor and actual governance quality.',
    'If divine sanction is real (the reading''s axiom), the constraint is legitimate coordination. If it is a narrative cover (the alternative), the constraint is pure extraction with a high theater ratio. This is the constitutive ambiguity of the monarchical reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_sanction_grounding, preference, 'Whether hereditary authority is legitimated by divine will or justified by it').

omega_variable(
    succession_stability_claim,
    'Does hereditary succession actually provide more stable and predictable rule than constitutional or republican alternatives? Or do constitutional systems solve the succession problem equally well without hereditary claims?',
    'Comparative institutional analysis: examine succession outcomes across monarchies and constitutional democracies. Measure: succession disputes, civil wars over succession, stability of succession rules over time.',
    'If hereditary succession is superior, the constraint''s coordination claim is vindicated and the high extraction is justifiable. If constitutional systems achieve equal or better stability without heredity, the constraint''s coordination claim is weaker and it appears more extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(succession_stability_claim, empirical, 'Whether hereditary succession is empirically superior to constitutional alternatives').

omega_variable(
    reading_boundary_ambiguity,
    'Is this constraint the MONARCHICAL READING of the kernel ''sovereign legitimacy'', or is it a different constraint (a historical ideology) being placed into the kernel framework retroactively?',
    'Examination of whether the monarchical doctrine explicitly interprets a stable text (a constitution, charter, or founding claim) or whether the reading is reconstructed from practice. If the former, the reading is genuine; if the latter, it may be a different constraint.',
    'This affects how to handle sibling relationships and whether the readings truly share a kernel or are merely similar-sounding constraints. A genuine kernel reading shares a stabilized text or commitment that different parties interpret; a reconstructed reading may not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether the monarchical principle is a genuine kernel reading or a historically reconstructed ideology').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.82) structural (legal barriers, enforcement machinery, exile) or partly internalized (subjects believe divine-right claims, making exit psychologically difficult even when legally possible)?',
    'Observation of post-constraint-collapse behavior: if subjects continue to defer to hereditary claims after the monarchy is deposed, suppression was internalized. If deference ceases, suppression was structural.',
    'Internalized suppression is harder to reverse and makes the constraint appear more like a mountain (natural acceptance) than a snare (artificial coercion). Structural suppression is cleaner extraction. The true picture likely mixes both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized belief').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sove_tr_t5, sovereign_legitimacy__monarchical_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(sove_tr_t10, sovereign_legitimacy__monarchical_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(sove_tr_t15, sovereign_legitimacy__monarchical_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__monarchical_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(sove_tr_t25, sovereign_legitimacy__monarchical_reading, theater_ratio, 25, 0.61).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__monarchical_reading, theater_ratio, 30, 0.61).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__monarchical_reading, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.61).
narrative_ontology:measurement(sove_be_t5, sovereign_legitimacy__monarchical_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(sove_be_t10, sovereign_legitimacy__monarchical_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(sove_be_t15, sovereign_legitimacy__monarchical_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__monarchical_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(sove_be_t25, sovereign_legitimacy__monarchical_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__monarchical_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__monarchical_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(sove_su_t5, sovereign_legitimacy__monarchical_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(sove_su_t10, sovereign_legitimacy__monarchical_reading, suppression_requirement, 10, 0.77).
narrative_ontology:measurement(sove_su_t15, sovereign_legitimacy__monarchical_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__monarchical_reading, suppression_requirement, 20, 0.81).
narrative_ontology:measurement(sove_su_t25, sovereign_legitimacy__monarchical_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__monarchical_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__monarchical_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__monarchical_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'sovereign legitimacy'. The republican and constitutional-hybrid readings are sibling constraints with the same kernel but different structural decompositions. All three share the referent (authority vesting in the sovereign) but differ on: (a) whether the sovereign is hereditary or popular, (b) whether legitimacy is grounded in divine right or in law, (c) whether authority flows downward from the crown or upward from consent. This decomposition is necessary because changing the reading changes the beneficiary structure, the ε value, and the suppression mechanisms. The readings are not observable-dependent variations on one constraint; they are genuinely different constraints instantiated by the same contested kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
