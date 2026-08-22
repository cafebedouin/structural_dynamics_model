% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__conceptual_emergence_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Copyright as Limited Regulatory Tool for Learning (Conceptual Emergence)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is read under this constraint as a moment of
 *   CONCEPTUAL EMERGENCE: the statute created a new legal category—copyright
 *   as a time-limited regulatory tool justified by public learning, not as a
 *   perpetual property right. This reading emphasizes the statute's power as
 *   a NAME-GIVING ACT. Before 1710, 'copyright' did not exist as a distinct
 *   concept; after 1710, it did. The constraint is the emergence of that
 *   category itself—the codification of the idea that intellectual property
 *   should be conceived as limited duration, author-benefiting, and
 *   subordinate to public benefit. This is distinct from the institutional
 *   reallocation reading (which emphasizes who holds rights) and the
 *   entangled event reading (which denies the separation of concept from
 *   event). This reading focuses purely on the conceptual shift: IP 'became
 *   thinkable' in a new way.
 *
 * KEY AGENTS:
 *   - statute_framers: Institutional agents (Parliament, Crown) who authored a new conceptual frame
 *   - public_learning: The vindicated outcome—beneficiary of a framework that treats IP as serving education
 *   - stationers_company: Institutional actor whose perpetual monopoly loses conceptual legitimacy
 *   - would_be_authors_and_printers: Moderate-power agents who gain a narrative position as beneficiaries
 *   - perpetual_monopoly_principle: The excluded conceptual frame—the reading declares it incoherent within the statute's space
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.18).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.05).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.12).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Copyright as Limited Regulatory Tool for Learning (Conceptual Emergence)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, '6aa10231-b265-42f8-87b4-11b910c8253c').
narrative_ontology:cs_kernel_codification('6aa10231-b265-42f8-87b4-11b910c8253c', fixed_text).
narrative_ontology:cs_authority_grounding('6aa10231-b265-42f8-87b4-11b910c8253c', lineage).
narrative_ontology:cs_interpretation_layer_present('6aa10231-b265-42f8-87b4-11b910c8253c').
narrative_ontology:cs_reading_relation('6aa10231-b265-42f8-87b4-11b910c8253c', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('6aa10231-b265-42f8-87b4-11b910c8253c', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('6aa10231-b265-42f8-87b4-11b910c8253c', foundational, copyright_is_categorically_distinct_from_perpetual_privilege).
narrative_ontology:cs_axiom_status(copyright_is_categorically_distinct_from_perpetual_privilege, holdable).
narrative_ontology:cs_axiom_grounding('6aa10231-b265-42f8-87b4-11b910c8253c', copyright_is_categorically_distinct_from_perpetual_privilege, deontological).
narrative_ontology:cs_axiom('6aa10231-b265-42f8-87b4-11b910c8253c', foundational, limited_duration_is_intrinsic_to_copyright_concept).
narrative_ontology:cs_axiom_status(limited_duration_is_intrinsic_to_copyright_concept, holdable).
narrative_ontology:cs_axiom_grounding('6aa10231-b265-42f8-87b4-11b910c8253c', limited_duration_is_intrinsic_to_copyright_concept, conventional).
narrative_ontology:cs_axiom('6aa10231-b265-42f8-87b4-11b910c8253c', secondary, public_learning_justifies_copyright_regulation).
narrative_ontology:cs_axiom_status(public_learning_justifies_copyright_regulation, holdable).
narrative_ontology:cs_axiom_grounding('6aa10231-b265-42f8-87b4-11b910c8253c', public_learning_justifies_copyright_regulation, instrumental).
narrative_ontology:cs_reference_frame('6aa10231-b265-42f8-87b4-11b910c8253c', pre_statute_perpetual_monopoly_frame).
narrative_ontology:cs_drift_state('6aa10231-b265-42f8-87b4-11b910c8253c', contemporary_ip_doctrine, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6aa10231-b265-42f8-87b4-11b910c8253c', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, knowledge_circulation_ecosystem).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, would_be_authors_and_printers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, intellectual_property_as_regulatory_category).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, limited_duration_doctrine).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_benefit_rationale_for_ip).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Parliament and Crown agents who authored the Statute of Anne in 1710, responding to the Stationers' monopoly petition and public outcry over high book prices. They created a new legal category—limited copyright—that framed intellectual property as a regulatory tool to encourage learning rather than a perpetual property right. They established the fourteen-year term and reversion-to-authors principle as the kernel of this new space.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_framers, agenda_setter,
    institutional, generational, analytical, national).

% The aggregate benefit accruing to readers, scholars, printers, and future authors from the existence of a conceptual framework that treats IP as a limited right serving public education rather than as perpetual monopoly. This is not an actor but a vindicated outcome—the constraint instantiates a principle that public learning is the rationale for copyright's existence.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning).

% The systemic capacity for knowledge to move through society without indefinite blocking by copyright holders. The statute's conceptual frame enables this ecosystem by defining copyright as a tool subordinate to learning, not as property that trumps access. This is a structural outcome, not a party.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, knowledge_circulation_ecosystem, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__conceptual_emergence_reading, knowledge_circulation_ecosystem).

% The London guild that held a de facto perpetual monopoly on book printing and trade under Crown privilege. The statute's conceptual emergence—framing copyright as limited and regulatory—directly undermines their framing of the monopoly as a natural property right. They lose the conceptual ground that would have allowed them to sustain perpetual privilege; instead, the statute names their privilege as a contingent regulatory arrangement. They are excluded from the new conceptual space because that space defines their position as extractive rather than legitimate.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company, excluded).

% Persons outside the Stationers' Company who want to publish but are excluded by the monopoly. The statute's conceptual frame gives them a narrative position—they become the beneficiaries of a regulatory tool designed to help them, rather than obstacles to a natural property right. The fourteen-year term and author reversion create a legal path into publishing that did not exist before, at least in principle.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, would_be_authors_and_printers, beneficiary,
    moderate, biographical, constrained, national).

% Scholars and jurists who witness the statute's articulation of IP as a distinct conceptual category. They observe that the statute instantiates a new point in the space of possible regulatory arrangements—copyright as time-limited, author-benefiting, and justified by public benefit rather than natural property. This observation becomes foundational to all subsequent IP doctrine.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legal_and_political_theorists, observer,
    analytical, generational, analytical, global).

% The conceptual framework that would have allowed the Stationers' monopoly to persist indefinitely as natural law or inherent right. The statute's emergence precludes this frame—perpetual monopoly becomes thinkable only as an explicit claim to property, not as an inevitable consequence of privilege. The principle is excluded from the statute's conceptual space by the frame that defines copyright as limited.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_principle, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_principle).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, diffuse).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__conceptual_emergence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a new legal category—copyright—that enables licensing and limited monopoly as a tool for encouraging authorship and learning, displacing the prior frame of perpetual guild monopoly as the only model for controlling reproduction. The statute coordinates multiple parties (authors, printers, readers, the Crown) around a new understanding of what intellectual property IS and what it is FOR.
% TRANSFER_FUNCTION: Transfers the conceptual basis of authority from the Stationers' guild (which claimed perpetual privilege via Crown grant as natural or inevitable) to a regulatory frame where the Crown and Parliament explicitly choose the duration and beneficiaries of copyright. The statute moves the justification from 'privilege granted by the Crown' to 'limited right created by statute to serve learning.' It also transfers the narrative position of would-be authors: from 'excluded by monopoly' to 'beneficiaries of a regulatory tool designed to help them.'
% ABSENT_VOICES: Readers and future authors who exist beyond the statute's time horizon are structurally absent—the statute cannot hear them. Their interest in accessing knowledge after the fourteen-year term, or in building on existing works, is not represented in the framing. The statute's beneficiaries (public learning) are generic and unorganized; they cannot petition Parliament. The Stationers' Company speaks loudly for perpetual privilege; no organized counterparty speaks for knowledge circulation until centuries later (and by then the frame is locked in).
% DISAPPEARANCE_RATIONALE: If the statute's conceptual emergence vanished—if copyright were never named as a distinct category or were immediately dissolved back into perpetual guild monopoly—the intellectual property landscape would reorganize around perpetual privilege claims as the default frame. The absence of a 'time-limited, learning-justified' conceptual category would mean that all subsequent debate about IP would lack the vocabulary to contest perpetuity. The statutory frame itself becomes a pivot point: its disappearance removes the conceptual tool that makes limited copyright thinkable.
% FOUNDING_PROBLEM: The Stationers' Company monopoly made books expensive and scarce, limiting learning; the Crown granted perpetual privilege but was besieged by petitions claiming the monopoly violated common right. The statute frames the founding problem as: 'How can the Crown and Parliament regulate reproduction to encourage both authorship AND learning, rather than treating reproduction as either a natural right or a perpetual monopoly?'
% FOUNDING_PROBLEM_CORROBORATION: Historians and archivists (outside the benefiting parties) attest the founding problem: the expense of books under the monopoly, the petitions against it, and the Crown's need to preserve privilege while responding to public outcry. The statute's preamble itself frames the problem—'to encourage learning'—but this statement is authored by the framers themselves (a benefiting party). The corroboration comes from independent historical records of the monopoly's effects and the political pressure that forced the statute's creation.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).
:- end_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW at 0.18 because the statute is conceived as a pure conceptual reframing with minimal direct extraction: the constraint is the new category itself, not the enforcement of copyright claims. Before 1710, extractiveness was HIGH (0.92) because the Stationers' monopoly extracted perpetual rents with no time limit and no public-benefit justification. At the 1710 moment, extractiveness drops because the statute redefines what extraction IS—it becomes visible as extraction (limited, justified by learning) rather than as natural property. After 1710, extractiveness ticks up slightly (0.22 by 1760, then back to 0.18 by 1800) as authors and printers realize they can use the new category to extract limited monopoly rents, but the frame remains: these extractions are NAMED as limited and regulatory, not perpetual. Suppression is VERY LOW because the statute does not suppress—it creates a conceptual space. The old monopoly required suppression (0.85) to block competing printers and silence competing claims; the statute eliminates the need for suppression by redefining the category. Theater is LOW (0.08) because the statute is substantially what it claims—a genuine conceptual shift, not performative. The measurement series shows a sharp drop at 1710 (the statute's passage), stability through the 18th century (the frame holds), and slight increases (reflecting the tension between the learning-justification and the monopoly-rent that the frame enables). Measurements at 1790 are projected because the historical record grows sparse; the basis field records this.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (statute framers) experiences this as a conceptual victory—they have created a new category that solves the political problem of legitimating copyright while constraining monopoly. The excluded Stationers' Company experiences it as a loss of conceptual ground—their frame (perpetual privilege) is no longer the only option. Would-be authors and readers experience it as a gain—they are now named as beneficiaries rather than obstacles. The engine should compute different types for these seats: the framers may read this as cooperative rope (coordination of parties around a new concept), while the Stationers read it as constraint they did not choose. The measurement series at 1710 captures the moment of divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The statute framers are agenda-setters with high institutional power and very low directionality—they set the frame and do not bear its extraction costs (d near 0.0). The public_learning beneficiary is generic and analytical—it exists as a vindicated outcome, not as an agent (agent: false). The Stationers' Company bears the cost of losing conceptual monopoly and narrative legitimacy, but their 'victim' status is peculiar: they are not exploited in the classical sense (their ability to print is not curtailed—they still hold the monopoly for the next 14 years), but the statute removes their claim to perpetual right. Their directionality is HIGH (d near 1.0) in the sense that the statute's emergence FORECLOSES their conceptual frame. Would-be authors and printers have MEDIUM directionality (d near 0.5)—the statute creates a category that can benefit them, but they must still navigate existing power structures to use it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint should NOT be classified as a mandatrophy case. The founding problem (monopoly's expense and public outcry) is LIVE at 1710 and remains LIVE through 1800—the statute's conceptual frame continues to address it by defining copyright as limited and learning-justified. The world would substantially rearrange if the statute disappeared: without the category 'limited copyright,' the debate would collapse back into perpetual-vs-natural-right. The statute avoids mandatrophy precisely because it keeps the founding problem LIVE by constantly instantiating the idea that copyright is a regulatory tool, not perpetual property. However, there is a subtle decay: over time (1760-1800), the extractive component grows as authors and printers realize they can use the new category to collect rents. This is not mandatrophy but CONCEPTUAL DRIFT—the frame persists but its content shifts. The statute's emergence reading does not predict this drift; it only captures the moment of emergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_priority_over_institutional,
    'Is the statute''s primary historical effect CONCEPTUAL (it created a new category, ''limited copyright'') or INSTITUTIONAL (it redistributed rights from guild to individual authors)? Can these be disentangled?',
    'Textual analysis of contemporary responses to the statute (1710-1730): do commentators emphasize the novelty of ''time-limited copyright as a regulatory tool'' or the power shift from Stationers to authors? Do legal treatises cite the statute for its conceptual innovation or for its institutional reallocation? Archival evidence of how jurists and reformers used the statute in subsequent IP disputes would clarify which aspect dominated their reasoning.',
    'If the conceptual frame dominates, this reading is correct and the statute''s primary function is NAME-GIVING. If institutional reallocation dominates, the institutional_reallocation_reading captures the true driver of change. If neither dominates—if the statute''s effect was to entangle them irreparably—the entangled_event_reading wins and both single-factor readings fail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_priority_over_institutional, conceptual, 'Whether the statute''s primary effect is conceptual or institutional; whether they can be separated.').

omega_variable(
    retroactive_categorization_risk,
    'Is ''copyright as a limited regulatory tool'' a genuine concept that emerged AT the statute''s moment, or a retrospective CATEGORY we impose on the statute when reading it through later IP doctrine? Did the statute-framers themselves think in these terms?',
    'Historical discourse analysis: what language did the statute''s preamble, Parliament, and contemporary commentators use to justify the statute? Do they use the phrase ''limited copyright'' or ''regulatory tool''? Or do they use older language (''privilege,'' ''grant,'' ''monopoly'')? If older language dominates, the categorization may be anachronistic.',
    'If the statute-framers used ''limited copyright'' language, the conceptual emergence reading is vindicated. If they used only older language, the conceptual category may be a projection we impose, and the statute becomes better read as institutional reallocation with later reframing. This would weaken the emergence claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retroactive_categorization_risk, conceptual, 'Whether ''limited copyright as regulatory tool'' is a historical concept or a retrospective analytical category.').

omega_variable(
    beneficiary_agency_and_organization,
    'Who is ''public learning''? The statute names it as a beneficiary, but it has no voice, no organization, no ability to defend or maintain the frame. How durable is a conceptual frame whose beneficiary cannot resist attacks on it?',
    'Institutional history: which organized parties (authors, printers, readers'' societies, libraries, booksellers) were able to invoke the statute''s ''learning'' rationale in disputes over copyright scope? Did unorganized ''learning'' ever become an effective political voice, or did the copyright frame always depend on organized actors (authors, publishers) to maintain it? If unorganized, the frame''s durability is contingent on those organized parties'' interests aligning with it.',
    'If ''public learning'' remains unorganized and passive, the statute''s conceptual frame may be fragile—it persists only as long as organized parties find it useful, not because it has inherent legitimacy. This would suggest the frame is more OF a rope (coordination benefit for organized parties) than a pure conceptual emergence benefiting a diffuse public.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_agency_and_organization, empirical, 'Whether ''public learning'' is an organized or unorganized beneficiary, and how this affects the frame''s durability.').

omega_variable(
    perpetual_monopoly_foreclosure,
    'Does the statute''s emergence of ''limited copyright'' FORECLOSE the continued viability of perpetual monopoly claims, or does it merely CREATE AN ALTERNATIVE FRAME that can coexist with perpetuity arguments?',
    'Legal history: after 1710, do advocates for perpetual copyright ever claim that perpetuity is NATURAL or INEVITABLE, or do they instead argue FOR perpetuity as a POLICY CHOICE within the regulatory framework the statute established? If the latter, perpetuity is no longer foreclosed—it becomes a live policy option rather than the only frame.',
    'If perpetual claims are foreclosed, this reading''s core claim is vindicated: the statute rendered perpetual monopoly unthinkable as natural law. If perpetuity remains a live claim, the statute may have only INFLUENCED perpetual arguments (forcing them to adopt regulatory language) without foreclosing them. This would shift the reading_relations edge from ''forecloses'' to ''influences'' relative to a hypothetical ''perpetual_monopoly_reading.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(perpetual_monopoly_foreclosure, empirical, 'Whether the statute forecloses or merely constrains perpetual monopoly claims.').

omega_variable(
    entanglement_vs_emergence_boundary,
    'Is the conceptual emergence (the naming of ''limited copyright'') genuinely SEPARABLE from the institutional reallocation (moving rights from guild to authors)? Or is the entangled_event_reading correct that the ''concept'' and ''institution'' are indissociable aspects of one event?',
    'Counterfactual analysis: could Parliament have created the institutional reallocation (removing Stationers'' perpetual monopoly, granting authors the right to petition for registration) WITHOUT the conceptual reframing of copyright as ''limited and learning-justified''? Or would authors and the public have refused the institutional change without the new conceptual frame? Conversely, could the frame emerge without the institutional change? If both are necessary to each other, they are entangled; if one could occur without the other, they are separable.',
    'If separable, this reading stands alone. If entangled, the entangled_event_reading may be correct, and both this reading and the institutional_reallocation_reading are artifacts of how we narrate a single, indivisible event. This is a fundamental question about whether the kernel itself can support multiple independent readings or whether the readings are interpretations of inseparable components.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(entanglement_vs_emergence_boundary, conceptual, 'Whether conceptual emergence and institutional reallocation are separable or entangled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 1700, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1700, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement_basis(stat_tr_t1700, observed).
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1710, 0.08).
narrative_ontology:measurement_basis(stat_tr_t1710, observed).
narrative_ontology:measurement(stat_tr_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1730, 0.08).
narrative_ontology:measurement_basis(stat_tr_t1730, observed).
narrative_ontology:measurement(stat_tr_t1760, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1760, 0.1).
narrative_ontology:measurement_basis(stat_tr_t1760, observed).
narrative_ontology:measurement(stat_tr_t1790, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1790, 0.12).
narrative_ontology:measurement_basis(stat_tr_t1790, projected).
narrative_ontology:measurement(stat_tr_t1800, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement_basis(stat_tr_t1800, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1700, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1700, 0.92).
narrative_ontology:measurement_basis(stat_be_t1700, observed).
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1710, 0.18).
narrative_ontology:measurement_basis(stat_be_t1710, observed).
narrative_ontology:measurement(stat_be_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1730, 0.16).
narrative_ontology:measurement_basis(stat_be_t1730, observed).
narrative_ontology:measurement(stat_be_t1760, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1760, 0.22).
narrative_ontology:measurement_basis(stat_be_t1760, observed).
narrative_ontology:measurement(stat_be_t1790, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1790, 0.28).
narrative_ontology:measurement_basis(stat_be_t1790, projected).
narrative_ontology:measurement(stat_be_t1800, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1800, 0.18).
narrative_ontology:measurement_basis(stat_be_t1800, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1700, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1700, 0.85).
narrative_ontology:measurement_basis(stat_su_t1700, observed).
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1710, 0.05).
narrative_ontology:measurement_basis(stat_su_t1710, observed).
narrative_ontology:measurement(stat_su_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1730, 0.05).
narrative_ontology:measurement_basis(stat_su_t1730, observed).
narrative_ontology:measurement(stat_su_t1760, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1760, 0.06).
narrative_ontology:measurement_basis(stat_su_t1760, observed).
narrative_ontology:measurement(stat_su_t1790, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1790, 0.08).
narrative_ontology:measurement_basis(stat_su_t1790, projected).
narrative_ontology:measurement(stat_su_t1800, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement_basis(stat_su_t1800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.06).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% The statute_of_anne_ip_foundation kernel is read in three structurally distinct ways: this reading (conceptual_emergence) emphasizes the statute's power to CREATE a new semantic category—'copyright as limited, learning-justified regulation'—that did not exist before 1710. The institutional_reallocation_reading emphasizes WHO HOLDS RIGHTS after 1710 (authors instead of Stationers), treating the statute as a power redistribution. The entangled_event_reading denies that concept and institution can be separated, arguing the statute is a single event whose apparent conceptual and institutional aspects are retrospective narrative artifacts. These three readings have different ε values (emergence: 0.18 as a pure coordination/naming function; reallocation: higher, as a redistributive constraint; entangled: contextual, depending on whether the reading foreclose or coexist). Each story carries its own beneficiaries, victims, and stakeholder set. The network links them because they share a kernel (the Statute of Anne) and each reading's emergence or stability has implications for the others' coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
