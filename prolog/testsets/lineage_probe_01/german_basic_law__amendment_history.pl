% ============================================================================
% CONSTRAINT STORY: german_basic_law__amendment_history
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_german_basic_law__amendment_history, []).

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
 *   constraint_id: german_basic_law__amendment_history
 *   human_readable: German Basic Law: Amendment History as Managed Constitutional Revision
 *   domain: constitutional_law/political_settlement
 *
 * SUMMARY:
 *   The German Basic Law (Grundgesetz) is fundamentally a record of managed
 *   constitutional revision. Rather than being a fixed foundational text
 *   preserved through interpretive innovation, the Basic Law has been amended
 *   64 times (as of 2024) to accommodate major political settlements:
 *   rearmament and NATO integration (1955), emergency powers during Cold War
 *   tensions (1968), reunification (1990), asylum policy restrictions (1993),
 *   and the debt brake (2009). Each amendment was bargained at the
 *   supermajority threshold (two-thirds of Bundestag and Bundesrat),
 *   converting informal political settlements into constitutional text. This
 *   reading sees the constraint as the mechanism that suppresses
 *   extra-constitutional adaptation (judicial reinterpretation, executive
 *   practice, informal Länder-federal negotiation) and forces major
 *   constitutional change through the formal Article 79 procedure with its
 *   two-thirds gate. The beneficiary is textual continuity — the principle
 *   that significant constitutional settlements should be locked into the
 *   written text rather than floating in interpretation or practice. The
 *   victims are informal constitutional mutation (the natural drift of
 *   constitutional meaning through practice) and extra-constitutional
 *   adaptation (the ability to respond to crises through executive action or
 *   judicial innovation without supermajority ratification). The
 *   extractiveness varies across amendments: early amendments (rearmament)
 *   had lower extractiveness because they addressed genuine institutional
 *   gaps; later amendments (debt brake) have higher extractiveness because
 *   they impose policy constraints through constitutional formalization. The
 *   theater ratio has declined over time — early postwar amendments involved
 *   substantial ceremonial ratification of Weimar lessons, while recent
 *   amendments are more purely instrumental policy-locking.
 *
 * KEY AGENTS:
 *   - Textual Continuity Regime: Abstract beneficiary — the constitutional principle that major settlements should be formally written into text rather than left to interpretation or practice
 *   - Supermajority Coalition: Concrete beneficiary (institutional/arbitrage) — the political coalitions that author each amendment gain the ability to lock their settlements into constitutional form, increasing durability and legitimacy
 *   - Extra-Constitutional Adaptation Mechanism: Primary victim (powerless/trapped) — informal constitutional change through judicial interpretation, executive practice, and common understanding is suppressed and channeled into the formal amendment procedure
 *   - Political Minorities: Secondary victim (moderate/constrained) — face the two-thirds requirement; cannot block amendments within the supermajority coalition but do participate in negotiated settlements
 *   - The Länder Collective: Institutional actor (organized/mobile) — constitutional federalism gives Länder formal role in amendment (Bundesrat) but actual veto power is limited by intergovernmental bargaining dynamics
 *   - Bundesverfassungsgericht: Institutional actor (institutional/arbitrage) — interprets what remains within judicial reach (non-eternity-clause questions) and enforces the boundary against informal mutation of eternity-protected principles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_basic_law__amendment_history, 0.52).
domain_priors:suppression_score(german_basic_law__amendment_history, 0.58).
domain_priors:theater_ratio(german_basic_law__amendment_history, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_basic_law__amendment_history, extractiveness, 0.52).
narrative_ontology:constraint_metric(german_basic_law__amendment_history, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(german_basic_law__amendment_history, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(german_basic_law__amendment_history, tangled_rope).
narrative_ontology:human_readable(german_basic_law__amendment_history, "German Basic Law: Amendment History as Managed Constitutional Revision").
narrative_ontology:topic_domain(german_basic_law__amendment_history, "constitutional_law/political_settlement").

domain_priors:requires_active_enforcement(german_basic_law__amendment_history).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(german_basic_law__amendment_history, '80c81760-277e-4850-bac0-a9f032d08669').
narrative_ontology:cs_kernel_codification('80c81760-277e-4850-bac0-a9f032d08669', fixed_text).
narrative_ontology:cs_authority_grounding('80c81760-277e-4850-bac0-a9f032d08669', lineage).
narrative_ontology:cs_interpretation_layer_present('80c81760-277e-4850-bac0-a9f032d08669').
narrative_ontology:cs_reading_relation('80c81760-277e-4850-bac0-a9f032d08669', german_basic_law__basic_rights, coexists_with).
narrative_ontology:cs_reading_relation('80c81760-277e-4850-bac0-a9f032d08669', german_basic_law__dignity_eternity, coexists_with).
narrative_ontology:cs_reading_relation('80c81760-277e-4850-bac0-a9f032d08669', german_basic_law__federal_construction, influences).
narrative_ontology:cs_reading_relation('80c81760-277e-4850-bac0-a9f032d08669', german_basic_law__militant_democracy, coexists_with).
narrative_ontology:cs_axiom('80c81760-277e-4850-bac0-a9f032d08669', foundational, textual_change_requires_supermajority).
narrative_ontology:cs_axiom_status(textual_change_requires_supermajority, holdable).
narrative_ontology:cs_axiom_grounding('80c81760-277e-4850-bac0-a9f032d08669', textual_change_requires_supermajority, conventional).
narrative_ontology:cs_axiom('80c81760-277e-4850-bac0-a9f032d08669', secondary, eternity_clause_forecloses_total_revision).
narrative_ontology:cs_axiom_status(eternity_clause_forecloses_total_revision, holdable).
narrative_ontology:cs_axiom_grounding('80c81760-277e-4850-bac0-a9f032d08669', eternity_clause_forecloses_total_revision, deontological).
narrative_ontology:cs_reference_frame('80c81760-277e-4850-bac0-a9f032d08669', formalized_settlement_through_supermajority).
narrative_ontology:cs_drift_state('80c81760-277e-4850-bac0-a9f032d08669', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('80c81760-277e-4850-bac0-a9f032d08669', '').
narrative_ontology:cs_kernel_id(german_basic_law__amendment_history, german_basic_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(german_basic_law__amendment_history, textual_continuity_regime).
narrative_ontology:constraint_beneficiary(german_basic_law__amendment_history, supermajority_coalition_builders).
narrative_ontology:constraint_victim(german_basic_law__amendment_history, informal_constitutional_mutation).
narrative_ontology:constraint_victim(german_basic_law__amendment_history, extra_constitutional_adaptation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXTRA-CONSTITUTIONAL ADAPTATION (SNARE) — The informal mechanism of constitutional change through judicial interpretation, executive practice, and Länder-federal negotiation cannot exit the Article 79 supermajority requirement once a matter is deemed constitutionally essential. Adaptation is suppressed into formal amendment channels where two-thirds bargaining power applies. No alternative pathway; full extraction borne by informal mutation mechanisms.
constraint_indexing:constraint_classification(german_basic_law__amendment_history, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLITICAL MINORITIES (TANGLED ROPE) — Constrained by the supermajority requirement to participate in two-thirds bargaining, but also beneficiary of the same mechanism when in the supermajority coalition. Each amendment (rearmament 1955, emergency powers 1968, reunification 1990, asylum 1993, debt brake 2009) required minorities to negotiate terms. Mixed coordination (enabling constitutional settlement) and extraction (locking minorities into outcomes they cannot block).
constraint_indexing:constraint_classification(german_basic_law__amendment_history, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPERMAJORITY COALITION (ROPE) — The constitutional architecture coordinates settlement via supermajority requirement. Each major political shift (rearmament, emergency powers, reunification) required the coalition to bring minorities into explicit negotiation. The two-thirds gate functions as pure coordination: benefits flow to all parties who participate in the settlement process, including the ability to claim constitutional legitimacy for crisis response.
constraint_indexing:constraint_classification(german_basic_law__amendment_history, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE LÄNDER COLLECTIVE (TANGLED ROPE) — Federalism gives Länder constitutional status and the Bundesrat role in amendment procedure. Länder can organize to block amendments (mobile exit via vetoing Bundesrat), but also coordinate federal-level crisis response through the same mechanism. Mixed: genuine coordination function (Bundesrat as site of federal-Länder negotiation) and asymmetric extraction (weak Länder have limited veto power in practice despite formal seat).
constraint_indexing:constraint_classification(german_basic_law__amendment_history, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: WEIMAR INSTITUTIONAL MEMORY (PITON) — The amendment mechanism exists partly because Weimar failed to constrain its own destruction. Article 79 is performative theater surrounding the substantive constraint (eternity clause protecting federalism, rights, dignity). The elaborate amendment procedure is maintained as ritual commemoration of constitutional defeat — maintained through institutional inertia and symbolic function rather than because the two-thirds supermajority constraint genuinely prevents mischief (majorities can and do amend). Theater ratio reflects that much amendment procedure is ceremonial ratification of political settlements made through other channels.
constraint_indexing:constraint_classification(german_basic_law__amendment_history, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The amendment history reading sees the Basic Law as a mechanism for locking political settlements into constitutional text. Each amendment (rearmament 1955, emergency powers 1968, reunification 1990, asylum 1993, debt brake 2009) converts a political bargain into a supermajority-protected constitutional rule. This reading reveals both the coordination function (settlements require legitimate buy-in) and the extraction mechanism (once locked into text, informal adaptation is suppressed). The mechanism works — settlements stick — but at the cost of foreclosing extra-constitutional flexibility and informal constitutional mutation.
constraint_indexing:constraint_classification(german_basic_law__amendment_history, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(german_basic_law__amendment_history_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(german_basic_law__amendment_history, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(german_basic_law__amendment_history, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(german_basic_law__amendment_history, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(german_basic_law__amendment_history, TR),
    TR >= 0.70.

:- end_tests(german_basic_law__amendment_history_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting the cost imposed on informal constitutional adaptation and on political minorities locked into supermajority settlements. The value has increased from 1949 (0.25 — early amendments addressed genuine gaps and were relatively consensual) through the 1990s reunification (0.55 — constitutional settlement required rapid formalization with minority input) and leveled at 2009 (0.52 — debt brake represented policy-locking with enduring minority opposition). The metric captures both the direct cost (minorities trapped in supermajority votes) and the structural cost (informal adaptation pathways foreclosed). Suppression (0.58): Moderate-high. The two-thirds requirement suppresses extra-constitutional adaptation by forcing formal amendment when informal change would otherwise occur. Judicial reinterpretation of rights can proceed only within the eternity-clause boundary; executive practice is constrained by Bundesverfassungsgericht enforcement of constitutional limits. Länder-federal accommodation occurs through the Bundesrat amendment process, not informal negotiation. However, suppression is not total — some adaptation (Article 3 equal protection jurisprudence, Article 5 free speech boundaries) has expanded through interpretation despite Article 79(3) protection. Theater ratio (0.38): Moderate. Early postwar amendments (rearmament, emergency powers) were heavily ritualized — performances of democratic self-restraint, Weimar lessons, constitutional legitimation of security measures. The 1990 reunification amendment was heavily ceremonial (constitutional ratification of a geopolitical settlement made through other channels). The 2009 debt brake and other recent amendments are more purely instrumental — direct policy-locking with minimal theatrical element. The declining trajectory reflects the amendment mechanism becoming normalized: from exceptional constitutional moments (1955, 1968) to routine policy formalization (2009 debt brake, 2012 fiscal compact incorporation).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Extra-constitutional adaptation sees a snare — it is trapped into formal amendment channels with no alternative pathway and no voice. Political minorities see tangled rope — they benefit from the legitimacy of supermajority settlements (buy-in, constitutional embedding) but bear the cost of two-thirds veto (inability to block outcomes). The supermajority coalition sees pure rope — the mechanism coordinates settlement and benefits all parties who participate. The Länder see tangled rope — they have formal constitutional role (Bundesrat participation) but limited practical veto power. Weimar institutional memory sees piton — the elaborate amendment ritual persists partly from constitutional paranoia (Weimar failed to constrain itself) but is increasingly performative rather than functionally necessary. The analytical observer sees tangled rope — the mechanism genuinely coordinates settlement (requiring buy-in) while imposing extraction costs (minorities locked in, informal adaptation suppressed). The maximum gap is between the snare perception (extra-constitutional adaptation) and the rope perception (supermajority coalition) — the same structure appears as entrapment to one agent and pure coordination to another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit options. Extra-constitutional adaptation (powerless/trapped victim) derives d ≈ 0.95 (full target, maximum f(d) ≈ 1.42), experiencing maximum effective extraction. Political minorities (moderate/constrained, mixed victim-beneficiary) derive d ≈ 0.55 (mixed participant), experiencing moderate extraction but also coordination benefit. Supermajority coalition (institutional/arbitrage, beneficiary) derives d ≈ 0.15 (beneficiary with high exit options), experiencing low extraction or negative χ (subsidy from the mechanism). The Länder (organized/mobile, mixed role) derive d ≈ 0.45 (constrained participant with some mobility), experiencing moderate extraction. Weimar memory (institutional/arbitrage) derives d ≈ 0.20 (beneficiary of ritual commemoration). The analytical observer (analytical/analytical, neutral position) derives d ≈ 0.72 (canonical analytical value), experiencing moderate structural extraction (the mechanism imposes costs on adaptation even to neutral observing). The directionality calculation is mechanism-specific: the same agent (e.g., a minority party) experiences different d values depending on whether it is calculating from within the supermajority coalition (beneficiary, lower d) or outside it (excluded, higher d). This is why the override mechanism exists — the schema's canonical fallback may not capture intra-institutional asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying what coordination function exists and what extraction it masks. The amendment_history reading reveals that the two-thirds supermajority requirement DOES coordinate constitutional settlement (forces buy-in from supermajority parties, increases legitimacy) while ALSO extracting from informal adaptation pathways and from minorities outside the supermajority coalition. This is the classic tangled_rope structure: genuine coordination mechanism (settlement requires supermajority blessing, not simple majority imposition) plus asymmetric extraction (minorities cannot block, informal pathways suppressed). The mechanism would be pure rope if minorities could arbitrage or exit (they cannot — the two-thirds gate is absolute for formal amendment). It would be pure snare if no coordination benefit existed (but the supermajority requirement is genuinely protective against simple-majority tyranny and does require meaningful negotiation). The mandatrophy dissolves when we recognize that the constraint is optimized for settlement durability (requiring wide buy-in) at the cost of flexibility (minorities locked in, informal adaptation suppressed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_boundary_negotiation,
    'What distinguishes a ''managed revision'' amendment (rearmament, emergency powers, reunification, asylum, debt brake) from extra-constitutional adaptation that should have remained informal?',
    'Historical analysis of constitutional disputes that were NOT amended (e.g., Bundesverfassungsgericht expansions of basic rights, executive war powers in limited deployments) versus those that WERE formalized. Comparison of amendment triggers: political crisis (yes), juridical pressure (sometimes), executive fait accompli (rarely).',
    'If the threshold is political crisis: amendments are reactive, not proactive — suppression of informal adaptation is a symptom, not a design feature. If threshold is contested constitutional principle: suppression is by design to prevent uncontrolled judicial expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_boundary_negotiation, empirical, 'What distinguishes amendments requiring supermajority from extra-constitutional adaptation').

omega_variable(
    eternity_clause_foreclosure,
    'Does the eternity clause (Article 79(3)) actually foreclose the competing dignity_and_eternity reading, or does amendment_history coexist with it as different aspects of the same kernel?',
    'Doctrinal analysis: do courts enforce Article 79(3) as an absolute barrier or as a rebuttable presumption? Can the Bundesverfassungsgericht reformulate the essence of a protected principle and allow amendment to change its application? Historical cases where dignity or federalism were reinterpreted without formal amendment.',
    'If foreclosure is absolute: amendment_history and dignity_and_eternity are incompatible readings (one must be wrong). If doctrinal boundary is porous: readings coexist — amendments *honor* the eternity clause while managing change through interpretation of what the clause protects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eternity_clause_foreclosure, conceptual, 'Whether eternity clause forecloses or coexists with amendment-history reading').

omega_variable(
    supermajority_arithmetic_asymmetry,
    'Does the two-thirds requirement actually suppress extra-constitutional adaptation, or does it primarily suppress minority veto power while allowing majority preference to crystallize?',
    'Comparison of amendment attempts (proposed but failed vs enacted): what percentage required supermajority? What percentage could have passed with simple majority? Historical moments when informal adaptation persisted because amendment was infeasible (two-thirds unavailable) versus moments when amendment succeeded despite minority opposition.',
    'If suppresses informal adaptation: victims are informal constitutional mutation and executive/judicial flexibility. If primarily suppresses minority veto: victims are the minorities themselves (trapped into settlements they cannot block), and extra-constitutional adaptation may continue outside the amendment frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajority_arithmetic_asymmetry, empirical, 'Whether two-thirds requirement suppresses informal adaptation or primarily suppresses minority veto').

omega_variable(
    readings_of_kernel_under_tension,
    'This constraint is one reading of the German Basic Law kernel. Are the sibling readings (basic_rights_catalog, dignity_and_eternity, federal_construction, militant_democracy) genuinely separate constraints, or do they describe aspects of the same constraint viewed from different angles?',
    'ε-invariance test: compute base_extractiveness for each reading under the same observable (German constitutional text + governance outcomes 1949–2024). If ε values differ by >0.15, they are separate constraints. If ε values cluster, they are perspectival views of one constraint and should be merged into a single story with multiple perspectives.',
    'If separate: the constraint family requires 5 separate JSON files linked by network.affects_constraints. If perspectival: this story should include additional perspectives (basic_rights, dignity, federalism, militant elements) and declare the kernel in commentary only, without reading_relations in cs_structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readings_of_kernel_under_tension, conceptual, 'Constraint family decomposition: amendment_history as separate constraint or as perspectival reading').

omega_variable(
    textual_continuity_beneficiary_definition,
    'Who or what exactly benefits from ''textual continuity under change''? Is it the constitutional order itself (abstract), the legal profession and courts that apply the text (institutional), the public trust in constitutional legitimacy (diffuse), or the particular political coalitions that author each amendment (concrete)?',
    'Operationalize benefit: measure legal citation authority (does amending via Article 79 increase the amendment''s precedential weight?), institutional trust surveys (do citizens perceive amended provisions as more legitimate?), political coalition durability (do settlements locked into text persist longer than informal understandings?), doctrinal clarity (do courts find amended provisions easier to apply than informal constitutional norms?).',
    'If abstract/institutional: extractiveness toward legal order itself, and victims include political flexibility and informal democratic adaptation. If concrete coalitions: extractiveness toward amendment-authors, and victims are minorities and future generations. Beneficiary definition changes who we classify as victim in directionality computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_continuity_beneficiary_definition, conceptual, 'Identity of beneficiary: textual continuity (abstract vs. concrete actor)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(german_basic_law__amendment_history, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gbl_amend_theater_1949, german_basic_law__amendment_history, theater_ratio, 0, 0.55).
narrative_ontology:measurement(gbl_amend_theater_1964, german_basic_law__amendment_history, theater_ratio, 15, 0.45).
narrative_ontology:measurement(gbl_amend_theater_1990, german_basic_law__amendment_history, theater_ratio, 41, 0.35).
narrative_ontology:measurement(gbl_amend_theater_1999, german_basic_law__amendment_history, theater_ratio, 50, 0.4).
narrative_ontology:measurement(gbl_amend_theater_2009, german_basic_law__amendment_history, theater_ratio, 60, 0.38).

% Extraction over time
narrative_ontology:measurement(gbl_amend_extr_1949, german_basic_law__amendment_history, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gbl_amend_extr_1964_emergency, german_basic_law__amendment_history, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(gbl_amend_extr_1990_reunif, german_basic_law__amendment_history, base_extractiveness, 41, 0.55).
narrative_ontology:measurement(gbl_amend_extr_1999_fiscalrule, german_basic_law__amendment_history, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(gbl_amend_extr_2009_debtbrake, german_basic_law__amendment_history, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(german_basic_law__amendment_history, enforcement_mechanism).
narrative_ontology:affects_constraint(german_basic_law__amendment_history, german_basic_law__basic_rights).
narrative_ontology:affects_constraint(german_basic_law__amendment_history, german_basic_law__dignity_eternity).
narrative_ontology:affects_constraint(german_basic_law__amendment_history, german_basic_law__federal_construction).
narrative_ontology:affects_constraint(german_basic_law__amendment_history, german_basic_law__militant_democracy).

% DUAL FORMULATION NOTE:
% The amendment_history reading is one of five decomposed readings of the German Basic Law kernel. Each reading (amendment_history, basic_rights_catalog, dignity_and_eternity, federal_construction, militant_democracy) focuses on a structurally distinct aspect of what the Basic Law IS. Amendment_history foregrounds the amendment process and constraint on written change; basic_rights_catalog foregrounds the catalog of binding rights; dignity_and_eternity foregrounds the unamendable foundations; federal_construction foregrounds the Länder and Bundesrat role; militant_democracy foregrounds the defense mechanisms. These are not perspectives on one constraint — they are separate constraints instantiated by different readings of the shared kernel. Each reading has its own ε, beneficiary/victim structure, and type classification. All five readings coexist: they are held by different jurisprudential schools, political actors, and scholarly traditions. The amendment_history reading affects all others because the amendment process is the mechanism through which all other aspects (rights, federalism, militant clauses) are formally embedded and protected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(german_basic_law__amendment_history, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
