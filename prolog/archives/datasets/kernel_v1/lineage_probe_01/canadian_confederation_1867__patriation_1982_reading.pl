% ============================================================================
% CONSTRAINT STORY: canadian_confederation_1867__patriation_1982_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canadian_confederation_1982_patriation, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: canadian_confederation_1867__patriation_1982_reading
 *   human_readable: Canadian Patriation (1982): Constitutional Autonomy Achieved, Quebec's Consent Bypassed
 *   domain: constitutional_law/political_legitimacy
 *
 * SUMMARY:
 *   Patriation (1982) finishes the founding begun in 1867 by bringing
 *   constitutional amendment authority from Westminster to Canada, and
 *   simultaneously inflicts a foundational wound by imposing the settlement
 *   without Quebec's consent. This constraint exemplifies how a single
 *   structural event can be simultaneously a coordination gain (domestic
 *   self-governance, Charter rights entrenchment) and an extraction mechanism
 *   (suppression of Quebec's consent authority, foreclosure of alternative
 *   amendment channels). The 1867 Confederation was a dominion granted by
 *   Westminster — incomplete as a sovereign founding because amendment
 *   authority remained in London. Patriation resolves this incompleteness,
 *   transferring amendment authority to a domestic amending formula and a
 *   supermajority of provinces. Yet the transfer was achieved by suppressing
 *   Quebec's effective veto and the Westminster parliamentary channel that
 *   might have required Quebec's consent. The wound is institutional: Quebec
 *   refused to sign; the settlement proceeded without it; yet Quebec remains
 *   bound by the new constitutional regime and retains formal veto power in
 *   future amendments (which constrains its utility as protest). The Charter
 *   creates enforceable rights with federal judicial enforcement, benefiting
 *   rights claimants and centralizing constitutional authority in federal
 *   courts. The amending formula protects provincial authority by requiring
 *   supermajorities for certain amendments, but this protection came at the
 *   cost of patriating without full provincial consensus. The constraint is
 *   tangled_rope because genuine coordination (domestic amendment authority,
 *   rights entrenchment, provincial veto protection) is embedded within
 *   extraction (Quebec's consent suppressed, Westminster channel foreclosed,
 *   federal power expanded through Charter enforcement). The unsignedness of
 *   Quebec is not incidental — it is the structural signature of this
 *   constraint, the marker that the founding was completed through extraction
 *   rather than consensus.
 *
 * KEY AGENTS:
 *   - Quebec Government: Primary victim (powerless/trapped, nationally scoped) — consent authority suppressed; bound by unsigned constitution; formal veto power constrained by political marginalization
 *   - Other Provincial Governments: Secondary victim/beneficiary (moderate/constrained) — gained amending formula veto protection but absorbed institutional strain from patriation without full consensus; forced to choose between supporting federal project and protecting Quebec's legitimacy claim
 *   - Federal Government: Primary beneficiary (institutional/arbitrage) — gained full domestic amendment authority; consolidated constitutional power; removed Westminster constraint; positioned as guarantor of Charter rights through federal judiciary
 *   - Charter Rights Claimants: Secondary beneficiary (organized/constrained) — gained enforceable constitutional rights; benefited from federal enforcement mechanism; but entrenchment also served federal expansion
 *   - Parliament of Westminster: Vestigial actor (institutional/arbitrage) — formally retained amendment role (Letter of Instruction) but bypassed in practice; piton classification reflects degraded institutional function
 *   - Canadian Public / Rights Movements: Tertiary beneficiary (organized/mobile) — benefited from Charter entrenchment and rights protection, though unequally (corporations, mobility rights expanded; Indigenous self-determination, provincial autonomy constrained)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing patriation as inevitable resolution of founding incompleteness; engine flags as false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canadian_confederation_1867__patriation_1982_reading, 0.52).
domain_priors:suppression_score(canadian_confederation_1867__patriation_1982_reading, 0.68).
domain_priors:theater_ratio(canadian_confederation_1867__patriation_1982_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canadian_confederation_1867__patriation_1982_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(canadian_confederation_1867__patriation_1982_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(canadian_confederation_1867__patriation_1982_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canadian_confederation_1867__patriation_1982_reading, tangled_rope).
narrative_ontology:human_readable(canadian_confederation_1867__patriation_1982_reading, "Canadian Patriation (1982): Constitutional Autonomy Achieved, Quebec's Consent Bypassed").
narrative_ontology:topic_domain(canadian_confederation_1867__patriation_1982_reading, "constitutional_law/political_legitimacy").

domain_priors:requires_active_enforcement(canadian_confederation_1867__patriation_1982_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(canadian_confederation_1867__patriation_1982_reading, '38360372-fcef-4a6f-b1cc-7da040ee2571').
narrative_ontology:cs_kernel_codification('38360372-fcef-4a6f-b1cc-7da040ee2571', formalized).
narrative_ontology:cs_authority_grounding('38360372-fcef-4a6f-b1cc-7da040ee2571', extraction).
narrative_ontology:cs_interpretation_layer_present('38360372-fcef-4a6f-b1cc-7da040ee2571').
narrative_ontology:cs_reading_relation('38360372-fcef-4a6f-b1cc-7da040ee2571', canadian_confederation_1867__notwithstanding_clause_reading, coexists_with).
narrative_ontology:cs_reading_relation('38360372-fcef-4a6f-b1cc-7da040ee2571', canadian_confederation_1867__peace_order_good_government_reading, influences).
narrative_ontology:cs_axiom('38360372-fcef-4a6f-b1cc-7da040ee2571', foundational, domestic_constitutional_self_governance).
narrative_ontology:cs_axiom_status(domestic_constitutional_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('38360372-fcef-4a6f-b1cc-7da040ee2571', domestic_constitutional_self_governance, deontological).
narrative_ontology:cs_axiom('38360372-fcef-4a6f-b1cc-7da040ee2571', foundational, consent_requirement_for_founding_legitimacy).
narrative_ontology:cs_axiom_status(consent_requirement_for_founding_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('38360372-fcef-4a6f-b1cc-7da040ee2571', consent_requirement_for_founding_legitimacy, deontological).
narrative_ontology:cs_reference_frame('38360372-fcef-4a6f-b1cc-7da040ee2571', dominion_incomplete_requiring_domestic_amendment).
narrative_ontology:cs_drift_state('38360372-fcef-4a6f-b1cc-7da040ee2571', contemporary_post_patriation_settlement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('38360372-fcef-4a6f-b1cc-7da040ee2571', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(canadian_confederation_1867__patriation_1982_reading, canadian_confederation_1867).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canadian_confederation_1867__patriation_1982_reading, canadian_federal_government).
narrative_ontology:constraint_beneficiary(canadian_confederation_1867__patriation_1982_reading, charter_rights_claimants).
narrative_ontology:constraint_beneficiary(canadian_confederation_1867__patriation_1982_reading, domestic_constitutional_autonomy).
narrative_ontology:constraint_victim(canadian_confederation_1867__patriation_1982_reading, quebec_consent_principle).
narrative_ontology:constraint_victim(canadian_confederation_1867__patriation_1982_reading, provincial_veto_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Quebec's constitutionally framed consent authority was suppressed by patriation without its signature. The province faces the constraint of a signed constitution it did not ratify, cannot exit, and cannot unilaterally amend. The suppression channel (Westminster amendment route closed; domestic amendment requires supermajority including Quebec) forecloses Quebec's exit to the prior regime. No legitimate exit option. Pure extraction of legitimacy.
constraint_indexing:constraint_classification(canadian_confederation_1867__patriation_1982_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Provincial governments experience mixed structure: the amending formula requires provincial consent for certain amendments, protecting provincial authority. Yet they also bore the suppression cost of patriating without Quebec — the legitimacy wound carries forward. They benefit from reduced federal overreach via the amending formula, but bear institutional strain from the unsigned province.
constraint_indexing:constraint_classification(canadian_confederation_1867__patriation_1982_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The federal government and Charter rights claimants are net beneficiaries. Patriation transfers constitutional authority from Westminster to Ottawa; the Charter creates a new rights-enforcement mechanism with federal judicial enforcement. The federal government gains full domestic amendment authority (through provincial consent plus supermajority mechanisms they help coordinate). Charter claimants gain an enforceable bill of rights. Both experience the constraint as coordination — bringing the constitution home and entrenching rights. The extraction runs toward them.
constraint_indexing:constraint_classification(canadian_confederation_1867__patriation_1982_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Westminster amendment channel (British Parliament ratifying Canadian constitutional changes) persists as a formalized vestige — the Statute of Westminster and Letter of Instruction are technically still operative for constitutional amendments, yet patriation bypassed them. The Westminster role has become performative theater, maintained through custom but superseded in function by domestic amendment mechanisms. The legitimacy of Westminster's role degrades as it is no longer invoked, yet the formality persists in doctrine.
constraint_indexing:constraint_classification(canadian_confederation_1867__patriation_1982_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a universal analytical perspective, the 1867 founding carried an inherent incompleteness: Confederation was a dominion granted by Westminster, not a sovereign act. Some analytical readings frame patriation as resolving an inevitable structural condition — that no founding can be complete until it becomes self-amending. However, the structural data reveals this as a false summit: the 'incompleteness' is contingent on the Westminster grant structure, not a natural law. The analytical mountain risks naturalizing what patriation itself shows to be a contestable institutional arrangement.
constraint_indexing:constraint_classification(canadian_confederation_1867__patriation_1982_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Organized rights movements and Charter architects (First Nations, women's movements, civil rights groups) experienced patriation as a temporary enabling mechanism: the Charter provided constitutional entrenchment of rights claims that could previously be modified by parliamentary majority. The scaffold logic: once Charter rights are sufficiently institutionalized in common law and judicial precedent, the extraordinary amending protection becomes background infrastructure. The sunset is implicit — as Charter culture matures, the special constitutional protection becomes conventional expectation rather than active enforcement.
constraint_indexing:constraint_classification(canadian_confederation_1867__patriation_1982_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canadian_confederation_1867__patriation_1982_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canadian_confederation_1867__patriation_1982_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canadian_confederation_1867__patriation_1982_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(canadian_confederation_1867__patriation_1982_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(canadian_confederation_1867__patriation_1982_reading, TR),
    TR >= 0.70.

:- end_tests(canadian_confederation_1867__patriation_1982_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The settlement achieved genuine coordination gains (domestic amendment authority, rights entrenchment, supermajority protection) but through suppression of Quebec's consent and Westminster's parliamentary channel. The extraction is not maximal because the coordination function is real and substantial — patriation was not pure rent extraction but rather a structural reorganization that benefited multiple parties. However, the extraction exceeds coordination because the beneficiary coalition (federal government + rights claimants) achieved its goals without requiring Quebec's signature, and the amending formula constrains future amendment in ways that benefit federal and established provincial interests at the cost of constitutional flexibility. Suppression (0.68): High. Multiple channels to assert alternatives were foreclosed: Quebec's legislative consent was suppressed; the Westminster parliamentary amendment route was bypassed; the prior dominion regime became irreversible (no exit to pre-1982 arrangements). The suppression is active enforcement (patriation required federal-provincial bargaining, Quebec's refusal was overridden through federal jurisdiction over amendment authority) and structural (Quebec remains locked into the new regime with no legitimate exit except constitutional renegotiation requiring the same parties). Theater ratio (0.55): Moderate. The amending formula and Charter entrenchment are substantively functional — they structure real amendment politics and rights adjudication, not mere ritual. Yet the patriation process itself involved performative elements: the Westminster channel was formalized away rather than abolished; Quebec's unsigned status was ritualized as a constitutional fact rather than resolved through consent; the domestic amendment mechanism was performed as consensus-based when consensus was actually partial and enforced. The theater has decreased from the dominion period (1867, theater_ratio = 0.65) as functional domestic amendment authority supplanted Westminster formalism, but it remains significant because the legitimacy claim (a founded constitution) was achieved without founding consensus.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint. Quebec sees snare (trapped, no exit, suppressed alternatives). The federal government sees rope (coordination gain, legitimate beneficiary). Other provinces see tangled rope (mixed protection and strain). Rights claimants see scaffold (temporary enabling mechanism on the path to institutionalized rights culture). Westminster sees piton (formal role persists but is functionally degraded and soon to be ceremonial). The analytical observer at the civilizational scope risks seeing mountain (founding incompleteness as natural limit resolved through inevitable structural adjustment). The engine flags this as a false summit: the 'incompleteness' is contingent on the Westminster grant structure and the dominion regime — a different founding architecture would have produced a different constraint. The perspectival gap reveals that patriation is best understood not as a natural law but as a contested settlement that benefited some parties (federal government, rights claimants) and wounded others (Quebec's consent authority) while generating coordination gains (domestic amendment, rights entrenchment) that were sufficient to stabilize the settlement despite the wound.
 *
 * DIRECTIONALITY LOGIC:
 *   Quebec as powerless/trapped: The province's power atom reflects its structural marginalization in the 1982 negotiation — it could not veto patriation itself, could not exit the resulting regime, and faced domestic political constraints (federalist vs. sovereigntist division) that prevented unified resistance. Its exit options are trapped because the constitutional regime is irreversible without the consent of parties (the federal government and other provinces) that have no incentive to renegotiate. Directionality d is high (approximately 0.92) because Quebec bears extraction costs (consent suppressed, formal veto constrained by political isolation) and gains minimal benefits from the settlement. Federal government as institutional/arbitrage: The federal actor holds arbitrage options (it could threaten constitutional stalemate, invoke federal override powers, negotiate alternative frameworks) and gained substantial benefits (amendment authority, Charter enforcement). Directionality d is low (approximately 0.12) because the federal government is the primary beneficiary and can exit the constraint if its interests are threatened. Other provinces as moderate/constrained: They benefit from the amending formula veto (protecting provincial jurisdiction) but are constrained by the need to maintain the constitutional settlement for broader legitimacy and by political pressure to isolate Quebec. Directionality d is approximately 0.55 (symmetric), reflecting both benefit (veto protection) and cost (institutional strain from unsigned province, reduced flexibility in future amendments).
 *
 * MANDATROPHY ANALYSIS:
 *   CONTESTED FOUNDING RESOLUTION: Patriation resolves mandatrophy by demonstrating that 'completing the founding' and 'inflicting extraction' are not mutually exclusive — the same structural event can instantiate both. The founding was incomplete (amendment authority in Westminster) and inefficient (dominion regime). Patriation completed it by domesticating amendment. Yet the completion was achieved through suppression of Quebec's consent and foreclosure of alternative amendment channels. The tangled_rope classification reflects this duality: genuine coordination (amending formula protecting provincial interests, Charter rights entrenchment) plus genuine extraction (Quebec's consent suppressed, Westminster channel foreclosed). The mandatrophy is not resolved by choosing one type; it is resolved by recognizing that the constraint instantiates both functions simultaneously. Quebec's powerless/trapped perspective (snare) is not wrong — Quebec genuinely bears extraction. The federal government's institutional/arbitrage perspective (rope) is also not wrong — the federal government genuinely gains coordination benefit and beneficiary status. The perspectival gap is not an error to be corrected but a structural feature of the settlement: it was achieved by parties with asymmetric power and interests, and those asymmetries are not erased by constitutional entrenchment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quebec_consent_legitimacy_threshold,
    'Does patriation without Quebec''s signature constitute a legitimacy deficit that compounds over time, or was it resolved by Quebec''s subsequent acquiescence and effective participation in constitutional politics?',
    'Analysis of Quebec''s constitutional participation and veto power post-1982; examination of whether the legitimacy wound remains active or has been absorbed into normal constitutional practice; measurement of ongoing Quebec resistance to the constitutional settlement vs. functional integration into amendment processes',
    'If unresolved: patriation remains a foundational extraction requiring reparative amendment (e.g., constitutional recognition of Quebec''s distinct status, Section 33 modification). If absorbed: the constraint classifies as resolved tangled_rope with dampened extractiveness as wounds are processed. If compounds: extractiveness increases over time as institutional strain accumulates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quebec_consent_legitimacy_threshold, empirical, 'Whether patriation''s legitimacy deficit persists or is absorbed through practice').

omega_variable(
    founding_completeness_framing,
    'Is patriation best understood as COMPLETING the founding (resolving an inevitable structural incompleteness of the 1867 dominion arrangement) or as REVISING the founding (imposing a new constitutional settlement that departed from the consensus required for founding legitimacy)?',
    'Doctrinal analysis of founding theory and constitutional legitimacy; comparison of patriation''s amendment basis (federal-provincial agreement, Westminster consent) to amendments of equal magnitude in other Westminster federations (Australia, etc.); examination of whether the 1867 founding texts anticipated or precluded unilateral patriation without full provincial consent',
    'If completion: the extraction framing weakens — patriation resolves a structural incompleteness that was always there. If revision: the extraction framing strengthens — patriation imposed a new settlement that required Quebec''s suppression to succeed. If ambiguous: the committer contest between readings persists irreducibly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_completeness_framing, conceptual, 'Whether patriation completes or revises the founding; locates the reading contest').

omega_variable(
    amending_formula_sufficiency,
    'Do the domestic amendment mechanisms (particularly the supermajority and provincial veto provisions) provide Quebec and other provinces sufficient structural protection to compensate for the suppression of their consent authority during patriation itself?',
    'Analysis of amendment vetoes exercised post-1982; comparison of provincial power under the amending formula vs. their power under pre-patriation Westminster arrangement; measurement of amendment proposals blocked or modified by provincial coalition power',
    'If sufficient: the constraint reclassifies from tangled_rope (mixed extraction and coordination) toward rope (pure coordination with embedded veto), because the amending formula is shown to be a genuine coordination gain protecting provincial interests. If insufficient: the extraction persists as provinces retain formal veto power but lack political leverage to exercise it effectively, and the amending formula is revealed as theater protecting institutional prerogatives rather than provincial autonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amending_formula_sufficiency, empirical, 'Whether amending formula provides structural compensation for consent suppression').

omega_variable(
    charter_rights_entrenchment_extraction,
    'Were Charter rights intended as an instrument of federal power expansion (leveraging judicial supremacy to override provincial jurisdiction), or as a genuine rights-protection mechanism independent of federal institutional benefit?',
    'Analysis of Charter jurisprudence on federal vs. provincial jurisdiction; comparison of pre-Charter and post-Charter federal overreach patterns; examination of which groups'' rights have been most expansively protected (corporations vs. marginalized groups; mobility rights vs. Indigenous self-determination)',
    'If federal expansion: the Charter-as-beneficiary framing reflects genuine extraction — rights entrenchment served as a cover for federal power consolidation. If genuine protection: the Charter represents a coordination gain for previously powerless rights claimants, offsetting the extraction of Quebec''s consent. If mixed: the constraint remains tangled_rope with the Charter representing both genuine coordination and embedded federal expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_rights_entrenchment_extraction, empirical, 'Whether Charter entrenchment served federal institutional expansion or rights protection').

omega_variable(
    westminster_ritual_sunset,
    'Is the continued formal role of Westminster (in Constitution Act doctrine, in the Letter of Instruction, in the theoretical amendment channel) likely to be formally abolished, or will it persist indefinitely as degraded ceremonial authority?',
    'Tracking of legislative proposals to formally sever Westminster amendment authority; analysis of whether the Westminster role is invoked in any substantial constitutional debates; comparison to other Westminster federations that have fully formalized domestic amendment channels and eliminated residual dominion-era language',
    'If formally abolished: the piton classification deepens as the Westminster vestige is stripped away, converting theater into explicit acknowledgment of non-functionality. If persists indefinitely: the piton remains stable, a perpetual artifact of incomplete decolonization embedded in Canadian constitutional doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(westminster_ritual_sunset, empirical, 'Whether Westminster''s formal constitutional role will be abolished or persist as ceremonial').

omega_variable(
    reading_contest_kernel_frame,
    'Which reading of the 1867 founding does patriation instantiate: the notwithstanding-clause reading (parliamentary sovereignty preserved inside a rights constitution), the patriation reading (completion and wound in one act), or the POGG reading (centralization on purpose)? Are these readings genuinely distinct or are they focal descriptions of the same structure from different angles?',
    'Doctrinal mapping of how each reading resolves Section 33 (notwithstanding clause), amending formula governance, and the Charter''s interaction with parliamentary sovereignty. Analysis of which reading(s) are invoked in judicial review, constitutional amendment politics, and legitimacy debates. Examination of whether the readings coexist peacefully or whether adopting one forecloses the others.',
    'If readings are distinct: patriation is best understood as one contingent choice among alternatives, and the extraction framing (suppression of alternatives) is analytically central. If readings are focal descriptions of the same thing: the extraction framing weakens, and patriation is a natural resolution point that different framings describe from different angles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_kernel_frame, conceptual, 'Locates this reading within the kernel contest and determines its relationship to sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canadian_confederation_1867__patriation_1982_reading, 1867, 1982).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1867_dominion_formalism, canadian_confederation_1867__patriation_1982_reading, theater_ratio, 1867, 0.65).
narrative_ontology:measurement(theater_1945_westminster_formalism_weakening, canadian_confederation_1867__patriation_1982_reading, theater_ratio, 1945, 0.58).
narrative_ontology:measurement(theater_1982_patriation_functional_shift, canadian_confederation_1867__patriation_1982_reading, theater_ratio, 1982, 0.55).

% Extraction over time
narrative_ontology:measurement(extractiveness_1867_dominion, canadian_confederation_1867__patriation_1982_reading, base_extractiveness, 1867, 0.15).
narrative_ontology:measurement(extractiveness_1945_westminster_bypass_pressure, canadian_confederation_1867__patriation_1982_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(extractiveness_1982_patriation_settlement, canadian_confederation_1867__patriation_1982_reading, base_extractiveness, 1982, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1867_dominion_founding, canadian_confederation_1867__patriation_1982_reading, suppression_requirement, 1867, 0.25).
narrative_ontology:measurement(suppression_1945_postwar_sovereignty_pressure, canadian_confederation_1867__patriation_1982_reading, suppression_requirement, 1945, 0.42).
narrative_ontology:measurement(suppression_1980_patriation_critical, canadian_confederation_1867__patriation_1982_reading, suppression_requirement, 1980, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canadian_confederation_1867__patriation_1982_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(canadian_confederation_1867__patriation_1982_reading, 0.12).
narrative_ontology:affects_constraint(canadian_confederation_1867__patriation_1982_reading, notwithstanding_clause_mechanism).
narrative_ontology:affects_constraint(canadian_confederation_1867__patriation_1982_reading, peace_order_good_government_federal_residue).
narrative_ontology:affects_constraint(canadian_confederation_1867__patriation_1982_reading, charter_rights_enforcement_hierarchy).
narrative_ontology:affects_constraint(canadian_confederation_1867__patriation_1982_reading, quebec_consent_legitimacy_deficit).

% DUAL FORMULATION NOTE:
% The patriation constraint is one node in a family of related constitutional constraints spanning 1867–1982. Upstream: the 1867 dominion regime with Westminster amendment authority (ε~0.15, primarily rope/mountain coordination). Siblings: notwithstanding_clause_reading (emphasizes Section 33 compromise, ε~0.40, scaffold); peace_order_good_government_reading (emphasizes federal centralization continuity, ε~0.35, tangled_rope). Downstream: contemporary Quebec's ongoing renegotiation pressure and constitutional reform debates grounded in patriation's extraction legitimacy. The three readings are not alternative observables of the same ε; they are genuinely distinct constraints with different ε values, beneficiary/victim structures, and temporal trajectories. They affect each other through constitutional doctrine — how courts resolve conflicts between Charter rights and Section 33, how federal residue doctrine applies to rights-related matters — but they have independent structural identities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(canadian_confederation_1867__patriation_1982_reading, institutional, 0.12).
constraint_indexing:directionality_override(canadian_confederation_1867__patriation_1982_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
