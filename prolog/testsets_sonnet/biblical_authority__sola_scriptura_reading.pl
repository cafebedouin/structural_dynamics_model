% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture Alone as Sufficient, Self-Interpreting Authority
 *   domain: theology/religious_studies/history
 *
 * SUMMARY:
 *   This story instantiates the sola scriptura reading of the biblical
 *   authority kernel: the claim that scripture, unaided by councils, popes,
 *   or magisterial tradition, is both sufficient and self-interpreting for
 *   doctrine and practice. Emerging from the Reformation's rejection of a
 *   perceived corrupt and extractive clerical hierarchy, this reading
 *   redistributes interpretive authority to the individual believer and the
 *   local congregation. Structurally this produces low clerical extraction
 *   and high congregational autonomy, but at the cost of any adjudicative
 *   mechanism to resolve interpretive disputes across communities — the
 *   doctrinal fragmentation into thousands of denominations is not an
 *   accident of this reading but a direct structural consequence of removing
 *   the single interpretive authority the sibling readings retain. This story
 *   does NOT evaluate the tradition_scripture_reading or conciliar_reading
 *   constraints; those are separate constraint files with their own epsilon
 *   values, beneficiary/victim structures, and classifications, linked here
 *   only through cs_structure.reading_relations and
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - individual_lay_believers: primary beneficiary (powerless/mobile) — gains direct interpretive authority
 *   - local_congregations: beneficiary and local agenda_setter (moderate/mobile) — sets its own doctrine autonomously
 *   - independent_pastors: beneficiary (moderate/mobile) — competes for adherents without ordination-lineage gatekeeping
 *   - cross_denominational_doctrinal_coherence: primary victim (non-agent, trapped) — no adjudicative mechanism exists to preserve it
 *   - councils_and_magisteria: excluded voice (institutional/trapped) — structurally denied a seat in this reading's authority chain
 *   - historical_theologians: analytical observer — traces the doctrine's fragmentation effects across five centuries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.28).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.22).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture Alone as Sufficient, Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies/history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '3f912de2-24ab-43ca-915b-7dafb698d30e').
narrative_ontology:cs_kernel_codification('3f912de2-24ab-43ca-915b-7dafb698d30e', fixed_text).
narrative_ontology:cs_authority_grounding('3f912de2-24ab-43ca-915b-7dafb698d30e', distributed).
narrative_ontology:cs_reading_relation('3f912de2-24ab-43ca-915b-7dafb698d30e', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('3f912de2-24ab-43ca-915b-7dafb698d30e', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('3f912de2-24ab-43ca-915b-7dafb698d30e', foundational, scripture_is_self_sufficient_and_self_interpreting).
narrative_ontology:cs_axiom_status(scripture_is_self_sufficient_and_self_interpreting, holdable).
narrative_ontology:cs_axiom_grounding('3f912de2-24ab-43ca-915b-7dafb698d30e', scripture_is_self_sufficient_and_self_interpreting, deontological).
narrative_ontology:cs_axiom('3f912de2-24ab-43ca-915b-7dafb698d30e', foundational, no_human_institution_holds_binding_interpretive_authority_over_the_text).
narrative_ontology:cs_axiom_status(no_human_institution_holds_binding_interpretive_authority_over_the_text, holdable).
narrative_ontology:cs_axiom_grounding('3f912de2-24ab-43ca-915b-7dafb698d30e', no_human_institution_holds_binding_interpretive_authority_over_the_text, conventional).
narrative_ontology:cs_reference_frame('3f912de2-24ab-43ca-915b-7dafb698d30e', apostolic_textual_sufficiency).
narrative_ontology:cs_drift_state('3f912de2-24ab-43ca-915b-7dafb698d30e', contemporary_denominational_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f912de2-24ab-43ca-915b-7dafb698d30e', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, individual_lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, local_congregations).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, vernacular_bible_publishers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, independent_pastors).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, cross_denominational_doctrinal_coherence).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, minority_dissenting_congregations).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, theologically_unsophisticated_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains direct, unmediated access to scripture and the authority to interpret it for their own conscience and practice, without requiring a priest, council, or magisterium to certify the reading. Can leave a congregation whose interpretation they reject and join or form another without ecclesiastical permission. Bears the cost of interpretive responsibility with limited formal training.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, individual_lay_believers, beneficiary,
    powerless, biographical, mobile, local).

% Operates with congregational autonomy, setting its own doctrinal positions and practices through its own reading of scripture rather than submitting to an external hierarchy. Free to split, merge, or reform around new interpretive consensus. This same freedom is what produces the doctrinal fragmentation that other actors experience as instability.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, local_congregations, beneficiary,
    moderate, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, local_congregations, agenda_setter).

% Derives authority from perceived fidelity to scripture and persuasive interpretive skill rather than ordination lineage or institutional appointment. Can found new congregations or denominations if their reading gains a following, with no adjudicative body able to block them. Competes for adherents in an open interpretive marketplace.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, independent_pastors, beneficiary,
    moderate, biographical, mobile, local).

% Profits from and is legitimated by the doctrine that every believer needs and can rightly interpret their own copy of scripture; translation, annotation, and study-bible markets expand because the reading validates lay engagement with the text without mediating authority.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, vernacular_bible_publishers, beneficiary,
    organized, generational, mobile, global).

% There is no adjudicative body positioned to resolve interpretive disputes when congregations diverge on baptism, communion, church governance, or salvation. Doctrinal splits multiply generationally with no structural mechanism for reconciliation short of voluntary re-union, which the same interpretive freedom that caused the split rarely produces.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, cross_denominational_doctrinal_coherence, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, cross_denominational_doctrinal_coherence).

% When their scriptural reading diverges from the local majority, they have no appeal to a higher interpretive authority to vindicate their position; their only recourse is exit and re-formation, which fragments further and can mean loss of community, property, and standing built over generations.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, minority_dissenting_congregations, payer,
    powerless, biographical, constrained, local).

% Told that scripture is self-interpreting (perspicuous) but lacking training in original languages, historical context, or textual criticism, they can be led into readings that serve a charismatic local leader's interests while believing they have arrived at the plain sense of the text themselves, with no external check available.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, theologically_unsophisticated_readers, payer,
    powerless, biographical, constrained, local).

% Structurally denied any adjudicative role in this reading; their historical claim to guard interpretation against error or resolve schism is treated as an unwarranted addition to scripture's own sufficiency. They would object that unmediated lay interpretation produces exactly the doctrinal chaos the conciliar and magisterial structures existed to prevent, but this reading gives them no seat.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, councils_and_magisteria, excluded,
    institutional, civilizational, trapped, global).

% Studies the doctrine's origins in the Reformation's rejection of papal and conciliar interpretive monopoly, and traces its downstream effect: thousands of denominations tracing descent from the same founding claim of scriptural sufficiency, each certain of its own plain reading.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes a single point of interpretive failure or capture (a corrupt or politically compromised hierarchy) by distributing interpretive authority to the text itself and to each believer's engagement with it, coordinating belief around a shared canonical source rather than a shared human institution.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy away from clerical hierarchies, councils, and magisteria and toward individual believers and congregations; moves the cost of doctrinal error from a centralized correcting body onto each community's own discernment.
% ABSENT_VOICES: Councils and magisteria are structurally excluded from this reading's authority chain; they would argue that unmediated interpretation without a check produces heresy and schism at scale, which is precisely what happened historically, but this reading's own framework gives their claim no standing to be heard.
% DISAPPEARANCE_RATIONALE: If sola scriptura were abandoned overnight in favor of a mandatory adjudicative authority, tens of thousands of independent congregations and denominations built on the premise of self-authorizing scriptural interpretation would face an immediate legitimacy crisis; publishing markets built on individual bible study, congregational polity structures, and non-hierarchical ordination pathways would all require restructuring around a reintroduced external authority.
% FOUNDING_PROBLEM: The Reformation-era problem of a papal and conciliar hierarchy perceived as doctrinally corrupt, financially extractive (indulgences), and resistant to internal correction, with no mechanism by which believers or lower clergy could challenge magisterial error using the shared textual source both sides claimed to honor.
% FOUNDING_PROBLEM_CORROBORATION: Reformation historians outside any single denomination corroborate that institutional corruption and resistance to correction were real and significant drivers. However, historians and comparative theologians also attest, from outside the sola scriptura tradition's own beneficiary set, that the doctrine has since generated a distinct second problem it was not built to solve — interpretive fragmentation without any mechanism for doctrinal reconciliation — which the tradition's own adherents rarely treat as a cost of the same commitment that solved the original problem.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at present) because this reading structurally removes concentrated clerical rent-collection — there is no tithe-collecting hierarchy whose authority depends on interpretive monopoly, and lay believers keep interpretive control rather than paying an institution for authorized readings. The gentle upward drift over the interval reflects the modern growth of independent megachurch and media-ministry structures that leverage sola scriptura's individual-authority framing to build large personal followings and revenue streams, a mild re-concentration within an otherwise decentralized structure. Suppression is low (0.22) because exit is genuinely available — a believer or congregation can leave and reconstitute elsewhere, which is the doctrine's own logic. Theater ratio is low and rising slowly (0.10 to 0.18), reflecting some growth in performative appeals to 'plain reading' that function more as rhetorical cover for a leader's preferred interpretation than as genuine textual engagement. Accessibility collapse is moderate (0.35): alternatives to this reading (submitting to a magisterium or council) remain fully visible and adopted by billions of other Christians, so collapse is far from mountain-like. Resistance is moderate-high (0.55): this reading has been contested since its formulation by conciliar and magisterial traditions who consider it doctrinally unstable by design.
 *
 * PERSPECTIVAL GAP:
 *   From the lay believer or independent congregation's seat, this reading computes near a rope: low extraction, genuine autonomy, minimal enforcement. From the seat of a minority dissenting congregation, or from the analytical seat tracking cross-denominational coherence, the same structure computes closer to a cost-bearing arrangement: fragmentation without recourse. The engine should register this divergence from the structural data (beneficiary concentration among individual/local actors vs. victim status of the powerless, non-agent 'coherence' entity) rather than from any authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual believers, congregations, independent pastors, and vernacular publishers sit near the beneficiary end of directionality: the reading removes an extractive intermediary and returns interpretive control (and in the publishers' case, market opportunity) to them directly. Cross-denominational doctrinal coherence, minority dissenting congregations, and theologically unsophisticated readers sit toward the target end: they bear the cost the beneficiaries do not — the first as a diffuse non-agent public good that erodes with each schism, the second and third as concrete groups exposed to interpretive drift or manipulation with no external check. Councils and magisteria are excluded rather than victimized in the extraction sense — their institutional standing is simply not recognized by this reading's framework, which is why they appear as excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a corrupt, correction-resistant hierarchy — was substantially real in the sixteenth century and is corroborated by historians outside the tradition's own beneficiary set. But the founding_problem_status is authored contested rather than dead or live because the doctrine now operates in a landscape where the original antagonist (a single corrupt hierarchy) has itself fragmented or reformed in many places, while the doctrine's own downstream cost — unresolvable doctrinal fragmentation — has become a second, distinct problem the doctrine has no internal mechanism to address. This is not classified as mandatrophy in the classic sense (a mandate persisting after its function died) because the coordination function — protecting against a single point of doctrinal capture — remains genuinely live wherever hierarchical religious authority is again perceived as corrupt or extractive; it is better read as an unresolved structural trade-off than an obsolete mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    perspicuity_versus_interpreter_expertise,
    'Is scripture actually self-interpreting (perspicuous) for the ordinary reader, or does the doctrine of perspicuity mask an unacknowledged dependency on the interpreter''s own theological formation, language, and cultural context — meaning the ''self-interpretation'' is really a hidden, unaccountable interpretive authority vested in whichever local leader shapes the reading?',
    'Comparative study of interpretive convergence versus divergence among lay readers given identical texts but different levels of theological training, controlling for denominational pre-formation; historical tracing of how frequently ''plain reading'' claims track a charismatic leader''s prior theological commitments rather than the text alone.',
    'If perspicuity is substantially real, this reading is closer to genuine rope (coordination without hidden extraction). If perspicuity functions mainly as legitimating cover for de facto pastoral authority, the reading understates its own clerical extraction and independent_pastors would need re-examination as a partial beneficiary of a hidden interpretive monopoly rather than a genuinely open marketplace participant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perspicuity_versus_interpreter_expertise, conceptual, 'Whether claimed textual self-sufficiency conceals de facto unaccountable local interpretive authority.').

omega_variable(
    fragmentation_cost_versus_correction_benefit,
    'Does the doctrinal fragmentation this reading produces represent a net cost (loss of a global correcting mechanism against error) or a net benefit (decentralized resilience against systemic capture, since no single point of hierarchical failure can corrupt the whole tradition at once)?',
    'Historical comparison of error-correction and resilience-to-corruption outcomes between traditions organized under this reading versus the conciliar and tradition-scripture readings across comparable multi-century periods, focusing on documented cases of institution-wide doctrinal capture versus documented cases of unrecoverable local schism.',
    'If fragmentation functions as protective decentralization, this reading''s victim declaration for doctrinal coherence should be weighted lower relative to its coordination benefit; if fragmentation functions mainly as uncorrected drift and manipulation risk, the victim declaration is understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fragmentation_cost_versus_correction_benefit, conceptual, 'Whether interpretive fragmentation is a protective feature or an uncorrected cost of removing adjudicative authority.').

omega_variable(
    kernel_framing_alternative_reading,
    'Is the correct unit of analysis ''sola scriptura as this reading declares it'' (text alone, self-interpreting), or is there a coherent alternative framing in which sola scriptura always operates alongside an implicit, unacknowledged interpretive tradition (creeds, catechisms, denominational confessions) that functions like a quasi-magisterium in practice even while denying magisterial authority in principle?',
    'Document the actual role of confessional statements, denominational catechisms, and seminary curricula within sola-scriptura-affirming traditions to assess whether a de facto tradition-layer exists that would, if made explicit, shift this constraint structurally toward the tradition_scripture_reading.',
    'If a de facto tradition layer is found to function as effectively adjudicative within particular denominations, that sub-population''s constraint would more accurately be modeled as a distinct hybrid story rather than a pure instance of this reading — a possible further decomposition under the epsilon-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_reading, conceptual, 'Whether confessional and catechetical structures constitute an unacknowledged quasi-magisterium within nominally sola-scriptura traditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 1517, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1517, biblical_authority__sola_scriptura_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement_basis(bibl_tr_t1517, observed).
narrative_ontology:measurement(bibl_tr_t1600, biblical_authority__sola_scriptura_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement_basis(bibl_tr_t1600, observed).
narrative_ontology:measurement(bibl_tr_t1750, biblical_authority__sola_scriptura_reading, theater_ratio, 1750, 0.13).
narrative_ontology:measurement_basis(bibl_tr_t1750, observed).
narrative_ontology:measurement(bibl_tr_t1900, biblical_authority__sola_scriptura_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t1900, observed).
narrative_ontology:measurement(bibl_tr_t1980, biblical_authority__sola_scriptura_reading, theater_ratio, 1980, 0.17).
narrative_ontology:measurement_basis(bibl_tr_t1980, observed).
narrative_ontology:measurement(bibl_tr_t2025, biblical_authority__sola_scriptura_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1517, biblical_authority__sola_scriptura_reading, base_extractiveness, 1517, 0.12).
narrative_ontology:measurement_basis(bibl_be_t1517, observed).
narrative_ontology:measurement(bibl_be_t1600, biblical_authority__sola_scriptura_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement_basis(bibl_be_t1600, observed).
narrative_ontology:measurement(bibl_be_t1750, biblical_authority__sola_scriptura_reading, base_extractiveness, 1750, 0.18).
narrative_ontology:measurement_basis(bibl_be_t1750, observed).
narrative_ontology:measurement(bibl_be_t1900, biblical_authority__sola_scriptura_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement_basis(bibl_be_t1900, observed).
narrative_ontology:measurement(bibl_be_t1980, biblical_authority__sola_scriptura_reading, base_extractiveness, 1980, 0.26).
narrative_ontology:measurement_basis(bibl_be_t1980, observed).
narrative_ontology:measurement(bibl_be_t2025, biblical_authority__sola_scriptura_reading, base_extractiveness, 2025, 0.28).
narrative_ontology:measurement_basis(bibl_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(biblical_authority__sola_scriptura_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__sola_scriptura_reading, 0.1).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the biblical_authority kernel (the others being tradition_scripture_reading and conciliar_reading), each a structurally distinct constraint with its own epsilon, beneficiary/victim structure, and classification per the epsilon-invariance principle. This reading shows the lowest authored extractiveness of the three (low clerical rent-extraction, high lay autonomy) and the highest authored doctrinal-fragmentation cost, since it alone lacks any adjudicative body. Historically this reading's emergence structurally influenced the persistence and self-justification pressure on both siblings, which had to respond to its critique of magisterial and conciliar authority; the edges here record that downstream influence, not shared identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
